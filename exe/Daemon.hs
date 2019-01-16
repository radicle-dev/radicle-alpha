{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daemon where

import           Protolude hiding (fromStrict, option)

import           Control.Monad.Except
--import           Control.Monad.Fail
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Network.HTTP.Types.URI as URL
import           Network.Wai.Handler.Warp (run)
import           Servant

import qualified Radicle.Internal.UUID as UUID
import           Server.Common

import           Radicle
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs

-- * Types

newtype IpnsId = IpnsId { ipnsId :: Text }
  deriving (A.ToJSON)

-- TODO(james): remove the protocol prefix
instance FromHttpApiData IpnsId where
  parseUrlPiece = Right . IpnsId . toS . URL.urlDecode False . toS
  parseQueryParam = parseUrlPiece

newtype JsonValue = JsonValue { jsonValue :: Value }

instance A.FromJSON JsonValue where
  parseJSON = A.withText "JsonValue" $ \t -> do
      v <- case parse "[daemon]" t of
        Left err -> fail $ "failed to parse Radicle expression: " <> show err
        Right v  -> pure v
      pure (JsonValue v)

instance A.ToJSON JsonValue where
  toJSON (JsonValue v) = A.String $ renderCompactPretty v

newtype Expression = Expression { expression :: JsonValue }
  deriving (Generic)

instance A.ToJSON Expression
instance A.FromJSON Expression

newtype Expressions = Expressions { expressions :: [JsonValue] }
  deriving (Generic)

instance A.FromJSON Expressions
instance A.ToJSON Expressions

data NewInput = NewInput
  { nonce :: Maybe Text }

data ReqInput = ReqInput
  { nonce      :: Maybe Text
  , expression :: [Value]
  }

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInput | Req ReqInput

-- * API

type Query = "query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression
type Send  = "send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] ()
type New   = "new"   :> Post '[JSON] IpnsId

type DaemonApi =
  "v1" :> "machines" :> ( Capture "machineId" IpnsId :> ( Query :<|> Send ) :<|> New )

serverApi :: Proxy DaemonApi
serverApi = Proxy

-- * Main

main :: IO ()
main = do
    let port = 8909 -- TODO(james): decide how/where/if port can be confugured
    -- TODO(james): - initAsReader all the reader chains
    --              - initAsWriter all the writer chains
    chains <- Chains <$> newMVar Map.empty
    let app = serve serverApi (server chains)
    logInfo "Start listening" [("port", show port)]
    run port app

type IpfsChain = Chain Ipfs.MachineEntryIndex
type IpfsChains = Chains Ipfs.MachineEntryIndex

server :: IpfsChains -> Server DaemonApi
server chains = machineEndpoints :<|> newMachine chains
  where
    machineEndpoints id = query chains id :<|> send chains id

-- * Endpoints

-- | Create a new IPFS machine and initialise the daemon as as the
-- /writer/.
newMachine :: IpfsChains -> Handler IpnsId
newMachine chains = do
  id_ <- liftIO $ Ipfs.ipfsMachineCreate =<< UUID.uuid -- TODO(james): may timeout, might be offline, etc.
  case id_ of
    Left err -> throwError $ err500 { errBody = fromStrict $ encodeUtf8 err }
    Right id -> do
      let m = emptyMachine id Writer
      liftIO $ insertMachine chains m
      _ <- withErr err500 $ initAsWriter chains (IpnsId id)
      -- TODO: check that we have persisted in config file that this
      -- daemon follows this machine.
      pure (IpnsId id)

-- | Evaluate an expression.
query :: IpfsChains -> IpnsId -> Expression -> Handler Expression
query chains id (Expression (JsonValue v)) = do
  m <- loadPinSubscribe chains id
  case fst <$> runIdentity $ runLang (chainState m) $ eval v of
    Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef err) }
    Right rv -> pure (Expression (JsonValue rv))

-- | Write a new expression to an IPFS machine.
send :: IpfsChains -> IpnsId -> Expressions -> Handler ()
send chains id (Expressions jvs) = do
  m <- loadPinSubscribe chains id
  case chainMode m of
    Writer -> do
      let vs = jsonValue <$> jvs
      -- TODO(james): remove duplication of below with addInputs
      case runIdentity $ runLang (chainState m) $ traverse eval vs of
        (Left err, _) -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef err) }
        (Right rs, newSt) -> do
          idx <- writeIpfs id vs
          let news = Seq.fromList $ zip vs rs
              m' = m { chainState = newSt
                     , chainEvalPairs = chainEvalPairs m Seq.>< news
                     , chainLastIndex = Just idx }
          liftIO $ insertMachine chains m'
          publish id (New NewInput{ nonce = Nothing })
          pure ()
    Reader -> do
      nonce <- liftIO $ UUID.uuid
      let isResponse = \case
            New NewInput{ nonce = Just nonce' } | nonce == nonce' -> True
            _ -> False
      -- TODO(james): decide what a good timeout is.
      msg_ <- liftIO $ subscribeOne id 1000 isResponse
      case msg_ of
        Nothing -> throwError $ err500 { errBody = fromStrict $ encodeUtf8 "The writer for this IPFS machine appears to be offline." }
        Just _ -> pure ()

-- * Helpers

type MachineState = Bindings (PrimFns Identity)

-- | Given an 'IpnsId', makes sure the machine state is materialised,
-- makes sure the inputs are pinned and subscribes to the topic for
-- updates. Returns the chain.
loadPinSubscribe :: IpfsChains -> IpnsId -> Handler IpfsChain
loadPinSubscribe chains id = do
  cs <- liftIO $ readMVar (getChains chains)
  case Map.lookup (ipnsId id) cs of
    -- In this case we have not seen the machine before, se we must be
    -- a reader.
    Nothing -> withErr err500 $ initAsReader chains id
    Just m -> case chainMode m of
      Writer -> pure m
      Reader -> do
        m' <- withErr err500 $ catchUpMachine chains m
        liftIO $ bumpPolling m'
        pure m'

-- Initialise the daemon service for this machine in reader mode.
initAsReader :: IpfsChains -> IpnsId -> ExceptT Text IO IpfsChain
initAsReader chains id = do
  m <- loadMachine Reader chains id
  liftIO $ actAsReader chains id
  liftIO $ bumpPolling m
  -- TODO: check that we have persisted in config file that this
  -- daemon follows this machine.
  pure m

initAsWriter :: IpfsChains -> IpnsId -> ExceptT Text IO IpfsChain
initAsWriter chains id = do
  m <- loadMachine Writer chains id
  liftIO $ actAsWriter chains id
  pure m

-- Loads a machine from IPFS.
loadMachine :: ReaderOrWriter -> IpfsChains -> IpnsId -> ExceptT Text IO IpfsChain
loadMachine mode chains id = do
  (idx, is) <- liftIO $ machineInputsFrom (ipnsId id) Nothing
  let m = emptyMachine (ipnsId id) mode
  addInputs chains m is idx

-- Given a cached machine, loads updates from IPFS.
catchUpMachine :: IpfsChains -> IpfsChain -> ExceptT Text IO IpfsChain
catchUpMachine chains m = do
  (idx, is) <- liftIO $ machineInputsFrom (chainName m) (chainLastIndex m)
  addInputs chains m is idx

-- Adds inputs to a cached machine.
addInputs :: IpfsChains -> IpfsChain -> [Value] -> Ipfs.MachineEntryIndex -> ExceptT Text IO IpfsChain
addInputs chains m is idx = case advanceChain m is of
    Left err -> ExceptT $ pure $ Left (renderCompactPretty err)
    Right (rs, newState) -> do
      let news = Seq.fromList $ zip is rs
          m' = m { chainState = newState
                 , chainLastIndex = Just idx
                 , chainEvalPairs = chainEvalPairs m Seq.>< news
                 }
      liftIO $ insertMachine chains m'
      pure m'

-- Subscribes the daemon to the machine's pubsub topic to listen for
-- input requests.
actAsWriter :: IpfsChains -> IpnsId -> IO ()
actAsWriter chains id = subscribeForever id onMsg
  where
    onMsg = \case
      Req ReqInput{..} -> pure () -- TODO(james): try to write the input and repond
      _ -> pure ()

-- Subscribes the daemon to the machine's pubsub topic to listen for
-- new input notification, and also sets up polling.
actAsReader :: IpfsChains -> IpnsId -> IO ()
actAsReader chains id = subscribeForever id onMsg
  where
    onMsg = \case
      New NewInput{..} -> pure () -- TODO(james): update materialised state, bump freq
      _ -> pure ()

-- Do some high-freq polling for a while.
bumpPolling :: IpfsChain -> IO ()
bumpPolling = notImplemented

-- TODO(james): rename to cacheMachine maybe
insertMachine :: IpfsChains -> IpfsChain -> IO ()
insertMachine chains chain = modifyMVar (getChains chains) $ \cs -> pure (Map.insert (chainName chain) chain cs, ())

emptyMachine :: Ipfs.IpnsId -> ReaderOrWriter -> IpfsChain
emptyMachine id mode =
  Chain{ chainName = id
       , chainState = pureEnv
       , chainEvalPairs = mempty
       , chainLastIndex = Nothing
       , chainMode = mode
       }

writeIpfs :: IpnsId -> [Value] -> Handler Ipfs.MachineEntryIndex
writeIpfs = notImplemented

-- | Publish a message on a machine's IPFS pubsub topic.
publish :: IpnsId -> Message -> Handler ()
publish = notImplemented

-- | Subscribe to messages on a machine's IPFS pubsub topic.
-- Takes an optional timeout.
subscribeForever :: IpnsId -> (Message -> IO ()) -> IO ()
subscribeForever = notImplemented

subscribeOne :: IpnsId -> Int -> (Message -> Bool) -> IO (Maybe Message)
subscribeOne = notImplemented

-- * IPFS helpers

machineInputsFrom :: Ipfs.IpnsId -> Maybe Ipfs.MachineEntryIndex -> IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom id idx = notImplemented -- Just <$> Ipfs.receiveIpfs id (Just idx)

-- TODO(james): There is no pinning anywhere.

withErr :: ServantErr -> ExceptT Text IO a -> Handler a
withErr code x_ = do
  x <- liftIO $ runExceptT x_
  case x of
    Left err -> throwError $ code { errBody = fromStrict $ encodeUtf8 err }
    Right y -> pure y
