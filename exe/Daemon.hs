{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daemon where

import           Protolude hiding (fromStrict, option)

import           System.Directory (doesFileExist)
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Network.HTTP.Types.URI as URL
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
import           Servant

import qualified Radicle.Internal.UUID as UUID
import           Server.Common

import           Radicle
import           Radicle.Daemon.Ipfs
import qualified Radicle.Internal.CLI as Local

-- * Types

instance A.ToJSON MachineId
instance A.FromJSON MachineId

-- TODO(james): remove the protocol prefix
instance FromHttpApiData MachineId where
  parseUrlPiece = Right . MachineId . toS . URL.urlDecode False . toS
  parseQueryParam = parseUrlPiece

newtype Expression = Expression { expression :: JsonValue }
  deriving (Generic)

instance A.ToJSON Expression
instance A.FromJSON Expression

newtype Expressions = Expressions { expressions :: [JsonValue] }
  deriving (Generic)

instance A.FromJSON Expressions
instance A.ToJSON Expressions

newtype SendResult = SendResult
  { results :: [JsonValue]
  } deriving (A.ToJSON)

-- * API

type Query = "query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression
type Send  = "send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] SendResult
type New   = "new"   :> Post '[JSON] MachineId

type DaemonApi =
  "v0" :> "machines" :> ( Capture "machineId" MachineId :> ( Query :<|> Send ) :<|> New )

daemonApi :: Proxy DaemonApi
daemonApi = Proxy

-- * Main

data Opts = Opts
  { port :: Int }

opts :: Parser Opts
opts = Opts
    <$> option auto
        ( long "port"
       <> help "daemon port"
       <> metavar "PORT"
       <> showDefault
       <> value 8909
        )

data Follows = Follows
  { follows :: [(MachineId, ReaderOrWriter)]
  } deriving (Generic)

-- TODO(james): write these by hand to make the file more
-- human-readable.
instance A.ToJSON Follows
instance A.FromJSON Follows

followFile :: IO FilePath
followFile = Local.getRadicleFile "daemon-follows"

type FollowFileLock = MVar ()

writeFollowFile :: FollowFileLock -> IpfsChains -> IO ()
writeFollowFile lock (Chains chainsVar) = do
  _ <- takeMVar lock
  ms <- takeMVar chainsVar
  ff <- followFile
  let fs = Follows $ second chainMode <$> Map.toList ms
  writeFile ff (toS $ A.encode fs)
  putMVar lock ()

readFollowFile :: FollowFileLock -> IO Follows
readFollowFile lock = do
  ff <- followFile
  _ <- takeMVar lock
  exists <- doesFileExist ff
  t <- if exists
    then readFile ff
    else let noFollows = toS (A.encode (Follows []))
         in writeFile ff noFollows *> pure noFollows
  case A.decode (toS t) of
    Nothing -> panic $ "Invalid daemon-follow file: could not decode " <> toS ff
    Just fs -> putMVar lock () *> pure fs

main :: IO ()
main = do
    Opts port <- execParser allOpts
    followFileLock <- newMVar ()
    Follows follows <- readFollowFile followFileLock
    chains <- Chains <$> newMVar Map.empty
    initRes <- runExceptT $ traverse_ (init chains) follows
    case initRes of
      Left err -> panic $ "Failed to initialise: " <> show err
      Right _ -> do
        let app = serve daemonApi (server followFileLock chains)
        logInfo "Start listening" [("port", show port)]
        run port app
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run then radicle daemon"
       <> header "radicle-daemon"
        )
    init chains (id, Reader) = initAsReader chains id
    init chains (id, Writer) = initAsWriter chains id

type IpfsChain = Chain MachineId MachineEntryIndex TopicSubscription
type IpfsChains = Chains MachineId MachineEntryIndex TopicSubscription

server :: FollowFileLock -> IpfsChains -> Server DaemonApi
server lock chains = machineEndpoints :<|> newMachine lock chains
  where
    machineEndpoints id = query lock chains id :<|> send lock chains id

-- * Endpoints

ll :: MonadIO m => Text -> m ()
ll = putStrLn

-- | Create a new IPFS machine and initialise the daemon as as the
-- /writer/.
newMachine :: FollowFileLock -> IpfsChains -> Handler MachineId
newMachine lock chains = do
  ll "new machine.."
  id_ <- createMachine
  ll (show id_)
  case id_ of
    Left err -> throwError $ err500 { errBody = fromStrict $ encodeUtf8 err }
    Right id -> daemonErr $ do
      sub <- initSubscription id
      let m = (emptyMachine id Writer sub)
      liftIO $ do
        insertMachine chains m
        actAsWriter chains m
        writeFollowFile lock chains
      pure id

-- | Evaluate an expression.
query :: FollowFileLock -> IpfsChains -> MachineId -> Expression -> Handler Expression
query lock chains id (Expression (JsonValue v)) = do
  m <- loadPinSubscribe lock chains id
  case fst <$> runIdentity $ runLang (chainState m) $ eval v of
    Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef err) }
    Right rv -> pure (Expression (JsonValue rv))

-- | Write a new expression to an IPFS machine.
send :: FollowFileLock -> IpfsChains -> MachineId -> Expressions -> Handler SendResult
send lock chains id (Expressions jvs) = do
  ll "send"
  m <- loadPinSubscribe lock chains id
  ll "loaded machine"
  case chainMode m of
    Writer -> do
      let vs = jsonValue <$> jvs
      (_, rs) <- daemonErr $ writeInputs chains m vs Nothing
      pure SendResult{ results = JsonValue <$> rs }
    Reader -> do
      nonce' <- liftIO $ UUID.uuid
      let isResponse = \case
            New NewInputs{ nonce = Just nonce'' } | nonce'' == nonce' -> True
            _ -> False
      -- TODO(james): decide what a good timeout is.
      msg_ <- liftIO $ subscribeOne (chainSubscription m) 5000 isResponse
      case msg_ of
        Nothing ->
          throwError $ err500 { errBody = fromStrict $ encodeUtf8 "The writer for this IPFS machine appears to be offline." }
        Just (New NewInputs{..}) ->
          pure SendResult{..}
        _ ->
          throwError $ err500 { errBody = fromStrict $ encodeUtf8 "Error: didn't filter machine topic messages correctly." }

-- * Helpers

lookupMachine :: (MonadIO m) => IpfsChains -> MachineId -> m (Maybe IpfsChain)
lookupMachine chains id = do
  cs <- liftIO $ readMVar (getChains chains)
  pure (Map.lookup id cs)

-- | Given an 'MachineId', makes sure the machine state is materialised,
-- makes sure the inputs are pinned and subscribes to the topic for
-- updates. Returns the chain.
loadPinSubscribe :: FollowFileLock -> IpfsChains -> MachineId -> Handler IpfsChain
loadPinSubscribe lock chains id = do
  m_ <- lookupMachine chains id
  case m_ of
    Nothing ->
      -- In this case we have not seen the machine before, se we must
      -- be a reader.
      daemonErr $ do
        m <- initAsReader chains id
        liftIO $ writeFollowFile lock chains
        pure m
    Just m -> case chainMode m of
      Writer -> pure m
      Reader -> do
        m' <- daemonErr $ catchUpMachine chains m
        liftIO $ bumpPolling m'
        pure m'

-- Initialise the daemon service for this machine in /reader mode/.
initAsReader :: IpfsChains -> MachineId -> ExceptT Error IO IpfsChain
initAsReader chains id = do
    ll $ "Init as reader: " <> show id
    m <- loadMachine Reader chains id
    _ <- liftIO $ addHandler (chainSubscription m) onMsg
    liftIO $ bumpPolling m
    pure m
  where
    onMsg = \case
      New NewInputs{..} -> do
        m_ <- lookupMachine chains id
        case m_ of
          Nothing ->
            logErr "Subscribed for changes on a non-loaded machine" [("machine_id", getMachineId id)]
          Just m  -> runExceptT (refreshAsReader chains m) >> pure ()
      _ -> pure ()

initAsWriter :: IpfsChains -> MachineId -> ExceptT Error IO IpfsChain
initAsWriter chains id = do
  ll $ "Init as writer: " <> show id
  m <- loadMachine Writer chains id
  liftIO $ actAsWriter chains m
  pure m

-- Loads a machine from IPFS.
loadMachine :: ReaderOrWriter -> IpfsChains -> MachineId -> ExceptT Error IO IpfsChain
loadMachine mode chains id = do
  (idx, is) <- machineInputsFrom id Nothing
  sub <- initSubscription id
  let m = emptyMachine id mode sub
  fst <$> addInputs chains m is (pure idx) (const (pure ()))

-- Given a cached machine, loads updates from IPFS.
catchUpMachine :: IpfsChains -> IpfsChain -> ExceptT Error IO IpfsChain
catchUpMachine chains m = do
  (idx, is) <- machineInputsFrom (chainName m) (chainLastIndex m)
  fst <$> addInputs chains m is (pure idx) (const (pure ()))

-- Add inputs to a cached machine.
addInputs
  :: IpfsChains
  -> IpfsChain
  -> [Value]
  -> ExceptT Error IO MachineEntryIndex -- ^ Determine the new index.
  -> ([Value] -> ExceptT Error IO ())   -- ^ Performed after the chain is cached.
  -> ExceptT Error IO (IpfsChain, [Value])
addInputs chains m is getIdx after = case advanceChain m is of
    Left err -> ExceptT $ pure $ Left $ InvalidInput err
    Right (rs, newState) -> do
      idx <- getIdx
      let news = Seq.fromList $ zip is rs
          m' = m { chainState = newState
                 , chainLastIndex = Just idx
                 , chainEvalPairs = chainEvalPairs m Seq.>< news
                 }
      liftIO $ insertMachine chains m'
      after rs
      pure (m', rs)

-- Write some inputs to a machine as the writer, sends out a
-- 'NewInput' message.
writeInputs :: IpfsChains -> IpfsChain -> [Value] -> Maybe Text -> ExceptT Error IO (IpfsChain, [Value])
writeInputs chains chain is nonce =
  let id = chainName chain
  in addInputs chains chain is (writeIpfs id is) (\rs -> publish id (New NewInputs{results = JsonValue <$> rs, ..}))

-- Subscribes the daemon to the machine's pubsub topic to listen for
-- input requests.
actAsWriter :: IpfsChains -> IpfsChain -> IO ()
actAsWriter chains m = addHandler (chainSubscription m) onMsg *> pure ()
  where
    id = chainName m
    onMsg = \case
      Req ReqInputs{..} -> do
        m_ <- lookupMachine chains id
        case m_ of
          Nothing -> logErr "Daemon was setup as writer for a chain it hasn't laoded!" [("machine_id", getMachineId id)]
          Just m' -> logErrors (writeInputs chains m' (jsonValue <$> expressions) (Just nonce) >> pure ())
      _ -> pure ()

refreshAsReader :: IpfsChains -> IpfsChain -> ExceptT Error IO IpfsChain
refreshAsReader chains m = do
  m' <- catchUpMachine chains m
  liftIO $ bumpPolling m'
  pure m'

-- Do some high-freq polling for a while.
bumpPolling :: IpfsChain -> IO ()
bumpPolling _ = pure () -- TODO: polling

-- TODO(james): rename to cacheMachine maybe
insertMachine :: IpfsChains -> IpfsChain -> IO ()
insertMachine chains chain = modifyMVar (getChains chains) $ \cs -> pure (Map.insert (chainName chain) chain cs, ())

emptyMachine :: MachineId -> ReaderOrWriter -> TopicSubscription -> IpfsChain
emptyMachine id mode sub =
  Chain{ chainName = id
       , chainState = pureEnv
       , chainEvalPairs = mempty
       , chainLastIndex = Nothing
       , chainMode = mode
       , chainSubscription = sub
       }

-- TODO(james): There is no pinning anywhere.

logErrors :: ExceptT Error IO () -> IO ()
logErrors x = do
  e_ <- runExceptT x
  case e_ of
    Left e -> logErr "Error" [("err", show e)] -- TODO(james): format errors depending on error.
    Right _ -> pure ()

daemonErr :: ExceptT Error IO a -> Handler a
daemonErr = Handler . withExceptT mapErr
  where
    mapErr = \case
      InvalidInput err -> err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef err) }
      IpfsError err -> err500 { errBody = toS err }
