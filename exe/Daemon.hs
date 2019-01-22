{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daemon where

import           Protolude hiding (fromStrict, option)

import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Network.HTTP.Types.URI as URL
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
import           Servant
import           System.Directory (doesFileExist)

import qualified Radicle.Internal.UUID as UUID
import           Server.Common

import           Radicle hiding (Env)
import           Radicle.Daemon.Ipfs
import qualified Radicle.Internal.CLI as Local


-- TODO(james): Check that the IPFS functions are doing all the
-- necessary pinning.

-- TODO(james): Polling.

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

data Error
  = InvalidInput (LangError Value)
  | IpfsError Text
  | AckTimeout
  | DaemonError Text
  deriving (Show)

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
  { port       :: Int
  , filePrefix :: Text
  }

opts :: Parser Opts
opts = Opts
    <$> option auto
        ( long "port"
       <> help "daemon port"
       <> metavar "PORT"
       <> showDefault
       <> value 8909
        )
    <*> strOption
        ( long "filePrefix"
       <> help "file prefix"
       <> metavar "PREFIX"
       <> showDefault
       <> value ""
        )

data Follows = Follows
  { follows :: [(MachineId, ReaderOrWriter)]
  } deriving (Generic)

-- TODO(james): write these by hand to make the file more
-- human-readable.
instance A.ToJSON Follows
instance A.FromJSON Follows

type FollowFileLock = MVar ()

data Env = Env
  { followFileLock :: FollowFileLock
  , followFile     :: FilePath
  , machines       :: IpfsChains
  }

newtype Daemon a = Daemon { fromDaemon :: ExceptT Error (ReaderT Env IO) a }
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO, MonadReader Env)

runDaemon :: Env -> Daemon a -> IO (Either Error a)
runDaemon env (Daemon x) = runReaderT (runExceptT x) env

withFollowFileLock :: FollowFileLock -> IO a -> IO a
withFollowFileLock lock act = bracket
  (takeMVar lock)
  (const (putMVar lock ()))
  (const act)

readFollowFileIO :: FollowFileLock -> FilePath -> IO Follows
readFollowFileIO lock ff = withFollowFileLock lock $ do
  exists <- doesFileExist ff
  t <- if exists
    then readFile ff
    else let noFollows = toS (A.encode (Follows []))
         in writeFile ff noFollows *> pure noFollows
  case A.decode (toS t) of
    Nothing -> panic $ "Invalid daemon-follow file: could not decode " <> toS ff
    Just fs -> pure fs

main :: IO ()
main = do
    opts' <- execParser allOpts
    followFileLock <- newMVar ()
    followFile <- Local.getRadicleFile (toS (filePrefix opts') <> "daemon-follows")
    machines <- Chains <$> newMVar Map.empty
    let env = Env{..}
    Follows follows <- readFollowFileIO followFileLock followFile
    initRes <- runDaemon env $ traverse_ init follows
    case initRes of
      Left err -> panic $ "Failed to initialise: " <> show err
      Right _ -> do
        let app = serve daemonApi (server env)
        logInfo "Start listening" [("port", show (port opts'))]
        run (port opts') app
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run then radicle daemon"
       <> header "radicle-daemon"
        )
    init (id, Reader) = initAsReader id
    init (id, Writer) = initAsWriter id

type IpfsChain = Chain MachineId MachineEntryIndex TopicSubscription
type IpfsChains = Chains MachineId MachineEntryIndex TopicSubscription

server :: Env -> Server DaemonApi
server env = hoistServer daemonApi nt daemonServer
  where
    daemonServer :: ServerT DaemonApi Daemon
    daemonServer = machineEndpoints :<|> newMachine
    machineEndpoints id = query id :<|> send id

    nt :: Daemon a -> Handler a
    nt d = do
      x_ <- liftIO $ runDaemon env d
      case x_ of
        Left err -> throwError (toServantErr err)
        Right x  -> pure x

    toServantErr :: Error -> ServantErr
    toServantErr = \case
      InvalidInput err -> err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef err) }
      IpfsError err -> err500 { errBody = toS err }
      DaemonError err -> err500 { errBody = toS err }
      AckTimeout -> err504 { errBody = "The writer for this IPFS machine does not appear to be online." }

-- * Endpoints

-- | Create a new IPFS machine and initialise the daemon as as the
-- /writer/.
newMachine :: Daemon MachineId
newMachine = do
  id_ <- createMachine
  case id_ of
    Left err -> throwError (IpfsError err)
    Right id -> do
      sub <- ipfs $ initSubscription id
      logInfo "Created new IPFS machine" [("id", getMachineId id)]
      let m = (emptyMachine id Writer sub)
      insertMachine m
      actAsWriter m
      writeFollowFile
      pure id

-- | Evaluate an expression.
query :: MachineId -> Expression -> Daemon Expression
query id (Expression (JsonValue v)) = do
  m <- loadPinSubscribe id
  case fst <$> runIdentity $ runLang (chainState m) $ eval v of
    Left err -> throwError (InvalidInput err)
    Right rv -> pure (Expression (JsonValue rv))

-- | Write a new expression to an IPFS machine.
send :: MachineId -> Expressions -> Daemon SendResult
send id (Expressions jvs) = do
  m <- loadPinSubscribe id
  case chainMode m of
    Writer -> do
      let vs = jsonValue <$> jvs
      (_, rs) <- writeInputs m vs Nothing
      pure SendResult{ results = JsonValue <$> rs }
    Reader -> do
      nonce' <- liftIO $ UUID.uuid
      let isResponse = \case
            New NewInputs{ nonce = Just nonce'' } | nonce'' == nonce' -> True
            _ -> False
      -- TODO(james): decide what a good timeout is.
      msg_ <- liftIO $ subscribeOne (chainSubscription m) 5000 isResponse
      case msg_ of
        Nothing -> throwError AckTimeout
        Just (New NewInputs{..}) -> pure SendResult{..}
        _ -> throwError $ DaemonError "Didn't filter machine topic messages correctly."

-- * Helpers

writeFollowFile :: Daemon ()
writeFollowFile = do
  lock <- asks followFileLock
  Chains msVar <- asks machines
  ff <- asks followFile
  liftIO $ withFollowFileLock lock $ do
    ms <- takeMVar msVar
    let fs = Follows $ second chainMode <$> Map.toList ms
    writeFile ff (toS $ A.encode fs)

lookupMachine :: MachineId -> Daemon (Maybe IpfsChain)
lookupMachine id = do
  ms <- asks machines
  cs <- liftIO $ readMVar (getChains ms)
  pure (Map.lookup id cs)

-- | Given an 'MachineId', makes sure the machine state is materialised,
-- makes sure the inputs are pinned and subscribes to the topic for
-- updates. Returns the chain.
loadPinSubscribe :: MachineId -> Daemon IpfsChain
loadPinSubscribe id = do
  m_ <- lookupMachine id
  case m_ of
    Nothing ->
      -- In this case we have not seen the machine before, se we must
      -- be a reader.
      do m <- initAsReader id
         writeFollowFile
         pure m
    Just m -> case chainMode m of
      Writer -> pure m
      Reader -> do
        m' <- catchUpMachine m
        liftIO $ bumpPolling m'
        pure m'

-- Initialise the daemon service for this machine in /reader mode/.
initAsReader :: MachineId -> Daemon IpfsChain
initAsReader id = do
    m <- loadMachine Reader id
    env <- ask
    _ <- liftIO $ addHandler (chainSubscription m) (daemonHandler env onMsg)
    logInfo "Following as reader" [("id", getMachineId id)]
    liftIO $ bumpPolling m
    pure m
  where
    onMsg :: Message -> Daemon ()
    onMsg = \case
      New NewInputs{..} -> do
        m_ <- lookupMachine id
        case m_ of
          Nothing -> logErr "Subscribed for changes on a non-loaded machine" [("id", getMachineId id)]
          Just m  -> refreshAsReader m >> pure ()
      _ -> pure ()

daemonHandler :: Env -> (Message -> Daemon ()) -> Message -> IO ()
daemonHandler env h msg = do
  x_ <- runDaemon env (h msg)
  case x_ of
    Left err -> logDaemonError err
    Right _  -> pure ()
  where
    logDaemonError err = logErr (show err) [] -- TODO: make betterer


initAsWriter :: MachineId -> Daemon IpfsChain
initAsWriter id = do
  m <- loadMachine Writer id
  actAsWriter m
  logInfo "Acting as writer" [("id", getMachineId id)]
  pure m

-- Loads a machine from IPFS.
loadMachine :: ReaderOrWriter -> MachineId -> Daemon IpfsChain
loadMachine mode id = do
  (idx, is) <- ipfsInputsFrom id Nothing
  sub <- ipfsSubscription id
  let m = emptyMachine id mode sub
  fst <$> addInputs m is (pure idx) (const (pure ()))

-- Given a cached machine, loads updates from IPFS.
catchUpMachine :: IpfsChain -> Daemon IpfsChain
catchUpMachine m = do
  (idx, is) <- ipfsInputsFrom (chainName m) (chainLastIndex m)
  fst <$> addInputs m is (pure idx) (const (pure ()))

ipfsInputsFrom :: MachineId -> Maybe MachineEntryIndex -> Daemon (MachineEntryIndex, [Value])
ipfsInputsFrom id = ipfs . machineInputsFrom id

ipfsSubscription :: MachineId -> Daemon TopicSubscription
ipfsSubscription = ipfs . initSubscription

-- Add inputs to a cached machine.
addInputs
  :: IpfsChain
  -> [Value]
  -> Daemon MachineEntryIndex -- ^ Determine the new index.
  -> ([Value] -> Daemon ())   -- ^ Performed after the chain is cached.
  -> Daemon (IpfsChain, [Value])
addInputs m is getIdx after = case advanceChain m is of
    Left err -> Daemon $ ExceptT $ pure $ Left $ InvalidInput err
    Right (rs, newState) -> do
      idx <- getIdx
      let news = Seq.fromList $ zip is rs
          m' = m { chainState = newState
                 , chainLastIndex = Just idx
                 , chainEvalPairs = chainEvalPairs m Seq.>< news
                 }
      insertMachine m'
      after rs
      pure (m', rs)

-- Write some inputs to a machine as the writer, sends out a
-- 'NewInput' message.
writeInputs :: IpfsChain -> [Value] -> Maybe Text -> Daemon (IpfsChain, [Value])
writeInputs m is nonce = addInputs m is write pub
  where
    id = chainName m
    write = ipfs $ writeIpfs id is
    pub rs = ipfs $ publish id (New NewInputs{results = JsonValue <$> rs, ..})

ipfs :: ExceptT Text IO a -> Daemon a
ipfs = Daemon . mapExceptT (lift . fmap (first IpfsError))

-- Subscribes the daemon to the machine's pubsub topic to listen for
-- input requests.
actAsWriter :: IpfsChain -> Daemon ()
actAsWriter m = do
    env <- ask
    liftIO $ addHandler (chainSubscription m) (daemonHandler env onMsg) *> pure ()
  where
    id = chainName m
    onMsg = \case
      Req ReqInputs{..} -> do
        m_ <- lookupMachine id
        case m_ of
          Nothing -> logErr "Daemon was setup as writer for a chain it hasn't laoded!" [("id", getMachineId id)]
          Just m' -> writeInputs m' (jsonValue <$> expressions) (Just nonce) >> pure ()
      _ -> pure ()

refreshAsReader :: IpfsChain -> Daemon IpfsChain
refreshAsReader m = do
  m' <- catchUpMachine m
  liftIO $ bumpPolling m'
  pure m'

-- Do some high-freq polling for a while.
bumpPolling :: IpfsChain -> IO ()
bumpPolling _ = pure () -- TODO: polling

-- TODO(james): rename to cacheMachine maybe
insertMachine :: IpfsChain -> Daemon ()
insertMachine chain = do
  ms <- asks machines
  liftIO $ modifyMVar (getChains ms) $ \cs -> pure (Map.insert (chainName chain) chain cs, ())

emptyMachine :: MachineId -> ReaderOrWriter -> TopicSubscription -> IpfsChain
emptyMachine id mode sub =
  Chain{ chainName = id
       , chainState = pureEnv
       , chainEvalPairs = mempty
       , chainLastIndex = Nothing
       , chainMode = mode
       , chainSubscription = sub
       }

-- logErrors :: Daemon () -> IO ()
-- logErrors e x = do
--   e_ <- runDaemon e x
--   case e_ of
--     Left e -> logErr "Error" [("err", show e)] -- TODO(james): format
--                                                -- errors depending on
--                                                -- error type.
--     Right _ -> pure ()
