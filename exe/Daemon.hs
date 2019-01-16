{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daemon where

import           Protolude hiding (fromStrict, option)

import           Control.Monad.Fail
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
    chains <- Chains <$> newMVar Map.empty -- TODO(james): The daemon should start by loading some known chains.
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

-- | Create a new IPFS machine as the /writer/.
newMachine :: IpfsChains -> Handler IpnsId
newMachine chains = do
  m_ <- liftIO $ Ipfs.ipfsMachineCreate =<< UUID.uuid
  case m_ of
    Left err -> throwError $ err500 { errBody = fromStrict $ encodeUtf8 err }
    Right id -> do
      let chain = emptyMachine id
      liftIO $ insertMachine chains chain
      _ <- loadPinSubscribe chains (IpnsId id)
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
    Nothing ->
      -- In this case we have not seen the machine before, se we must
      -- be the reader.
      followMachineAsReader chains id
    Just m -> do
      (idx, is) <- liftIO $ machineInputsFrom (ipnsId id) (chainLastIndex m)
      case advanceChain m is of
        Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef err) }
        Right (rs, newState) -> do
          let m' = Chain{ chainState = newState
                        , chainLastIndex = Just idx
                        , chainEvalPairs = notImplemented -- TODO(james):
                        }
          insertMachine chains m'
          -- TODO(james): In this case we know the machine se we are
          -- have the appropriate subscriptions setup. Only thing left
          -- to do is bump the polling.
          pure m'
 
followMachineAsReader :: IpfsChains -> IpnsId -> Handler IpfsChain
followMachineAsReader chains id = do
  (idx, is) <- liftIO $ machineInputsFrom (ipnsId id) Nothing
  let m = emptyMachine (ipnsId id) Reader
  case advanceChain m is of
    Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 (renderCompactPretty err) }
    Right (rs, newState) -> do
      let m' = m { chainState = newState
                 , chainLastIndex = Just idx
                 , chainEvalPairs = notImplemented -- TODO(james):
                 }
      liftIO $ insertMachine chains m'
      -- TODO(james): trigger subscribe and polling
      pure chain

-- TODO(james): rename to cacheMachine maybe
insertMachine chains chain = modifyMVar (getChains chains) $ \cs -> pure (Map.insert (chainName chain) chain cs, ())

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
subscribe :: IpnsId -> Maybe (Int, Message -> Bool) -> (Message -> IO ()) -> IO ()
subscribe = notImplemented

subscribeOne :: IpnsId -> Int -> (Message -> Bool) -> IO (Maybe Message)
subscribeOne = notImplemented

-- * IPFS helpers

machineInputsFrom :: Ipfs.IpnsId -> Maybe Ipfs.MachineEntryIndex -> IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom id idx = notImplemented -- Just <$> Ipfs.receiveIpfs id (Just idx)

-- TODO(james): There is no pinning anywhere.
