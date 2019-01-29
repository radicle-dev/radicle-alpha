-- | The radicle-daemon; a long-running background process which
-- materialises the state of remote IPFS machines on the user's PC, and
-- writes to those IPFS machines the user is an owner of.
--
-- See
-- <https://github.com/oscoin/radicle/blob/master/rfcs/0003-radicle-daemon.rst
-- the RFC>.
module Daemon where

import           Protolude hiding (fromStrict, option, poll)

import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Time.Clock.System as Time
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
import           Servant
import           System.Directory (doesFileExist)

import           Radicle.Daemon.Common hiding (logInfo)
import qualified Radicle.Daemon.Common as Common
import qualified Radicle.Internal.UUID as UUID

import           Radicle hiding (Env)
import           Radicle.Daemon.HttpApi
import           Radicle.Daemon.Ipfs
import qualified Radicle.Internal.CLI as Local
import qualified Radicle.Internal.ConcurrentMap as CMap


-- TODO(james): Check that the IPFS functions are doing all the
-- necessary pinning.

-- * Types

data MachineError
  = InvalidInput (LangError Value)
  | IpfsError IpfsError
  | AckTimeout
  | DaemonError Text
  | MachineAlreadyCached
  | MachineNotCached

data Error
  = MachineError MachineId MachineError
  | CouldNotCreateMachine Text

displayError :: Error -> (Text, [(Text,Text)])
displayError = \case
  CouldNotCreateMachine e -> ("Could not create IPFS machine", [("error", e)])
  MachineError id e -> let mid = ("machine-id", getMachineId id) in
    case e of
      InvalidInput err -> ("Invalid radicle input", [mid, ("error", renderCompactPretty err)])
      DaemonError err -> ("Internal error", [mid, ("error", err)])
      IpfsError e' -> case e' of
        IpfsDaemonError err -> ("There was an error using IPFS", [mid, ("error", toS (displayException err))])
        InternalError err -> ("Internal error", [mid, ("error", err)])
        NetworkError err -> ("There was an error communicating with the IPFS daemon", [mid, ("error", err)])
      AckTimeout -> ("The writer appears to be offline", [mid])
      MachineAlreadyCached -> ("Tried to add already cached machine", [mid])
      MachineNotCached -> ("Machine was not found in cache", [mid])

logDaemonError :: MonadIO m => Error -> m ()
logDaemonError (displayError -> (m, xs)) = logErr m xs

type Follows = Map MachineId ReaderOrWriter

-- * Main

data Opts = Opts
  { port       :: Int
  -- TODO(james): temporary, just for testing.
  , filePrefix :: Text
  , debug      :: Bool
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
    <*> switch
        ( long "debug"
       <> help "enable debug logging"
       <> showDefault
        )

type FollowFileLock = MVar ()

data LogLevel = Normal | Debug
  deriving (Eq, Ord)

data Env = Env
  { followFileLock :: FollowFileLock
  , followFile     :: FilePath
  , machines       :: IpfsMachines
  , logLevel       :: LogLevel
  }

newtype Daemon a = Daemon { fromDaemon :: ExceptT Error (ReaderT Env IO) a }
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO, MonadReader Env)

runDaemon :: Env -> Daemon a -> IO (Either Error a)
runDaemon env (Daemon x) = runReaderT (runExceptT x) env

main :: IO ()
main = do
    Opts{..} <- execParser allOpts
    followFileLock <- newMVar ()
    followFile <- Local.getRadicleFile (toS filePrefix <> "daemon-follows")
    machines <- Chains <$> CMap.empty
    let env = Env{ logLevel = if debug then Debug else Normal, ..}
    follows <- readFollowFileIO followFileLock followFile
    initRes <- runDaemon env (init follows)
    case initRes of
      Left err -> logErr "Init failed" [] >> logDaemonError err
      Right _ -> do
        polling <- async $ initPolling env
        let app = serve daemonApi (server env)
        Common.logInfo "Start listening" [("port", show port)]
        serv <- async $ run port app
        exc <- waitEitherCatchCancel polling serv
        case exc of
          Left (Left err) -> logErr "Polling failed with an exception" [("error", toS (displayException err))]
          Left (Right ()) -> logErr "Polling stopped (this should not happen)" []
          Right (Left err) -> logErr "Server failed with an exception" [("error", toS (displayException err))]
          Right (Right ()) -> logErr "Server stopped (this should not happen)" []
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run the radicle daemon"
       <> header "radicle-daemon"
        )

type IpfsMachine = Chain MachineId MachineEntryIndex TopicSubscription
type IpfsMachines = Chains MachineId MachineEntryIndex TopicSubscription

server :: Env -> Server DaemonApi
server env = hoistServer daemonApi nt daemonServer
  where
    daemonServer :: ServerT DaemonApi Daemon
    daemonServer = (machineEndpoints :<|> newMachine) :<|> pure swagger
    machineEndpoints id = query id :<|> send id

    nt :: Daemon a -> Handler a
    nt d = do
      x_ <- liftIO $ runDaemon env d
      case x_ of
        Left err -> throwError (toServantErr err)
        Right x  -> pure x

    toServantErr err@(MachineError _ e) = case e of
      InvalidInput e' -> err400 { errBody = fromStrict $ encodeUtf8 (renderPrettyDef e') }
      AckTimeout -> err504 { errBody = "The writer for this IPFS machine does not appear to be online." }
      _ -> case displayError err of
        (msg, _) -> err500 { errBody = toS msg }
    toServantErr (CouldNotCreateMachine err) = err500 { errBody = "Could not create IPFS machine: " <> toS err }

-- * Init

-- | Initiate machines according to follow file.
init :: Follows -> Daemon ()
init follows = traverse_ initMachine (Map.toList follows)
  where
    initMachine (id, Reader) = initAsReader id
    initMachine (id, Writer) = initAsWriter id

-- * Endpoints

-- | Create a new IPFS machine and initialise the daemon as as the
-- /writer/.
newMachine :: Daemon NewResponse
newMachine = do
    id <- create
    sub <- ipfs id $ initSubscription id
    logInfo Normal "Created new IPFS machine" [("machine-id", getMachineId id)]
    m <- liftIO $ emptyMachine id Writer sub
    insertNewMachine m
    actAsWriter m
    writeFollowFile
    pure (NewResponse id)
  where
    create = Daemon $ mapExceptT (lift . fmap (first CouldNotCreateMachine)) $ createMachine

-- | Evaluate an expression against a cached machine. The resulting
-- state is always discarded, and the expression is never sent to the
-- writer. Only used for local queries.
--
-- Hitting this endpoint will turn on high-frequency polling for a
-- fixed amount of time.
query :: MachineId -> QueryRequest -> Daemon QueryResponse
query id (QueryRequest v) = do
  m <- checkMachineLoaded id
  bumpPolling id
  case fst <$> runIdentity $ runLang (chainState m) $ eval v of
    Left err -> throwError $ MachineError id (InvalidInput err)
    Right rv -> do
      logInfo Normal "Query success:" [("result", renderCompactPretty rv)]
      pure (QueryResponse rv)

-- | Write a new expression to an IPFS machine.
--
-- - If the daemon is the writer for the machine, it will write the
--   new inputs to IPFS and then send out a notification on the
--   machine's pubsub topic.
--
-- - If the daemon is a reader for the machine, it will request the
--   machine's writer daemon to perform the write, and wait for an
--   ack.
send :: MachineId -> SendRequest -> Daemon SendResponse
send id (SendRequest expressions) = do
  mode_ <- machineMode
  case mode_ of
    Just (Writer, _) -> do
      results <- writeInputs id expressions Nothing
      pure SendResponse{..}
    Just (Reader, sub) -> requestInput sub
    Nothing -> do
      m <- initAsReader id
      requestInput (chainSubscription m)
  where
    requestInput sub = do
      nonce <- liftIO $ UUID.uuid
      let isResponse = \case
            New InputsApplied{ nonce = Just nonce' } | nonce' == nonce -> True
            _ -> False
      asyncMsg <- liftIO $ async $ subscribeOne sub ackWaitTime isResponse (logNonDecodableMsg id)
      ipfs id $ publish id (Submit SubmitInputs{..})
      logInfo Debug
              "Sent input request to writer"
              [ ("machine-id", getMachineId id)
              , ("expressions", prValues expressions) ]
      msg_ <- liftIO $ waitCatch asyncMsg
      case msg_ of
        Right (Just (New InputsApplied{results})) -> do
          logInfo Normal
                 "Writer accepted input request"
                 [ ("machine-id", getMachineId id)
                 , ("results", prValues results)
                 ]
          bumpPolling id
          pure SendResponse{..}
        Right Nothing -> throwError $ MachineError id AckTimeout
        Right _ -> throwError $ MachineError id (DaemonError "Didn't filter machine topic messages correctly.")
        Left err -> throwError $ MachineError id (DaemonError (toS (displayException err)))

    machineMode = fmap (liftA2 (,) chainMode chainSubscription) <$> lookupMachine id
    prValues = T.intercalate "," . (renderCompactPretty <$>)

-- * Helpers

logInfo :: LogLevel -> Text -> [(Text,Text)] -> Daemon ()
logInfo l msg infos = do
  l' <- asks logLevel
  if l <= l'
    then Common.logInfo msg infos
    else pure ()

withFollowFileLock :: FollowFileLock -> IO a -> IO a
withFollowFileLock lock = withMVar lock . const

readFollowFileIO :: FollowFileLock -> FilePath -> IO Follows
readFollowFileIO lock ff = withFollowFileLock lock $ do
  exists <- doesFileExist ff
  t <- if exists
    then readFile ff
    else let noFollows = toS (A.encode (Map.empty :: Follows))
         in writeFile ff noFollows $> noFollows
  case A.decode (toS t) of
    Nothing -> panic $ "Invalid daemon-follow file: could not decode " <> toS ff
    Just fs -> pure fs

writeFollowFile :: Daemon ()
writeFollowFile = do
  lock <- asks followFileLock
  Chains cMap <- asks machines
  ff <- asks followFile
  liftIO $ withFollowFileLock lock $ do
    ms <- CMap.nonAtomicRead cMap
    let fs = Map.fromList $ second chainMode <$> Map.toList ms
    writeFile ff (toS $ A.encode fs)

lookupMachine :: MachineId -> Daemon (Maybe IpfsMachine)
lookupMachine id = do
  msVar <- asks machines
  liftIO $ CMap.lookup id (getChains msVar)

-- | Given an 'MachineId', makes sure the machine is in the cache and
-- updated.
checkMachineLoaded :: MachineId -> Daemon IpfsMachine
checkMachineLoaded id = do
  m_ <- lookupMachine id
  case m_ of
    Nothing -> do
      -- In this case we have not seen the machine before so we act as
      -- a reader.
      m <- initAsReader id
      writeFollowFile
      pure m
    Just m -> case chainMode m of
      Writer -> pure m
      Reader -> refreshAsReader id
        -- TODO(james): For the moment we will just force a
        -- refresh. Later consider brining back the check to see if
        -- the machine was very recently updated.

        -- do
        -- delta <- liftIO $ sinceLastUpdate m
        -- -- If machine is half a second fresh, then return it.
        -- if delta < 500
        --   then pure m
        --   else refreshAsReader id

logNonDecodableMsg :: MachineId -> Text -> IO ()
logNonDecodableMsg (MachineId id) bad =
  Common.logInfo "Non-decodable message on machine's pubsub topic" [("machine-id", id), ("message", bad)]

daemonHandler :: Env -> MachineId -> (Message -> Daemon ()) -> Either Text Message -> IO ()
daemonHandler _ id _ (Left bad) = logNonDecodableMsg id bad
daemonHandler env _ h (Right msg) = do
  x_ <- runDaemon env (h msg)
  case x_ of
    Left err -> logDaemonError err
    Right _  -> pure ()

-- | Loads a machine fresh from IPFS.
loadMachine :: ReaderOrWriter -> MachineId -> Daemon IpfsMachine
loadMachine mode id = do
  (idx, is) <- ipfs id $ machineInputsFrom id Nothing
  sub <- ipfs id $ initSubscription id
  m <- liftIO $ emptyMachine id mode sub
  (m', _) <- addInputs is (pure idx) (const (pure ())) m
  insertNewMachine m'
  pure m'

-- | Add inputs to a cached machine.
addInputs
  :: [Value]
  -- ^ Inputs to add.
  -> Daemon MachineEntryIndex
  -- ^ Determine the new index.
  -> ([Value] -> Daemon ())
  -- ^ Performed after the chain is cached.
  -> IpfsMachine
  -> Daemon (IpfsMachine, ([Value], MachineEntryIndex))
  -- ^ Returns the updated machine, the results and the new index.
addInputs is getIdx after m =
  case advanceChain m is of
    Left err -> throwError $ MachineError (chainName m) (InvalidInput err)
    Right (rs, newState) -> do
      idx <- getIdx
      t <- liftIO $ Time.getSystemTime
      let news = Seq.fromList $ zip is rs
          m' = m { chainState = newState
                 , chainLastIndex = Just idx
                 , chainEvalPairs = chainEvalPairs m Seq.>< news
                 , chainLastUpdated = t
                 }
      after rs
      pure (m', (rs, idx))

ipfs :: MachineId -> ExceptT IpfsError IO a -> Daemon a
ipfs id = Daemon . mapExceptT (lift . fmap (first (MachineError id . IpfsError)))

-- | Do some high-freq polling for a while.
bumpPolling :: MachineId -> Daemon ()
bumpPolling id = do
  modifyMachine id $ \m' -> pure (m' { chainPolling = highFreq }, () )
  logInfo Debug "Reset to high-frequency polling" [("machine-id", getMachineId id)]

-- | Insert a new machine into the cache. Errors if the machine is
-- already cached.
insertNewMachine :: IpfsMachine -> Daemon ()
insertNewMachine m = do
    msVar <- asks machines
    inserted_ <- liftIO $ CMap.insertNew id m (getChains msVar)
    case inserted_ of
      Just () -> pure ()
      Nothing -> throwError (MachineError id MachineAlreadyCached)
  where
    id = chainName m

-- | Modify a machine that is already in the cache. Errors if the
-- machine isn't in the cache already.
modifyMachine :: MachineId -> (IpfsMachine -> Daemon (IpfsMachine, a)) -> Daemon a
modifyMachine id f = do
    env <- ask
    res <- liftIO $ CMap.modifyExistingValue id (getChains (machines env)) $ \m -> do
      x <- runDaemon env (f m)
      pure $ case x of
        Left err      -> (m, Left err)
        Right (m', y) -> (m', Right y)
    case res of
      Nothing         -> throwError (MachineError id MachineNotCached)
      Just (Left err) -> throwError err
      Just (Right y)  -> pure y

emptyMachine :: MachineId -> ReaderOrWriter -> TopicSubscription -> IO IpfsMachine
emptyMachine id mode sub = do
  t <- Time.getSystemTime
  pure Chain{ chainName = id
            , chainState = pureEnv
            , chainEvalPairs = mempty
            , chainLastIndex = Nothing
            , chainMode = mode
            , chainSubscription = sub
            , chainLastUpdated = t
            , chainPolling = highFreq
            }

-- ** Reader

-- | Load a machine in /reader mode/ and return it.
--
-- Loads the machines input log from IPFS and listens for machine
-- updates on pubsub.
--
-- The machine is added to the deamons machine cache.
initAsReader :: MachineId -> Daemon IpfsMachine
initAsReader id = do
    m <- loadMachine Reader id
    env <- ask
    _ <- liftIO $ addHandler (chainSubscription m) (daemonHandler env id onMsg)
    logInfo Normal "Following as reader" [ ("machine-id", getMachineId id)
                                         , ("current-input-index", show (chainLastIndex m)) ]
    pure m
  where
    onMsg :: Message -> Daemon ()
    onMsg = \case
      New InputsApplied{..} -> do
        _ <- refreshAsReader id
        -- TODO(james): decide if this should bump polling. If this is
        -- a very active chain this will mean the polling is always
        -- high-frequency.
        bumpPolling id
      _ -> pure ()

-- | Updates a machine in the cache with the new inputs pulled from IPFS.
refreshAsReader :: MachineId -> Daemon IpfsMachine
refreshAsReader id = modifyMachine id $ \m -> do
    let currentIdx = chainLastIndex m
    (newIdx, is) <- ipfs id $ machineInputsFrom (chainName m) currentIdx
    if currentIdx == Just newIdx
      then do
        logInfo Debug
                "Reader is already up to date"
                [mid, ("input-index",  show newIdx)]
        pure (m, m)
      else do
        (m', _) <- addInputs is (pure newIdx) (const (pure ())) m
        logInfo Debug
                "Updated reader"
                [mid, ("n", show (length is)), ("input-index",  show newIdx)]
        pure (m', m')
  where
    mid = ("machine-id", getMachineId id)

-- ** Writer

initAsWriter :: MachineId -> Daemon IpfsMachine
initAsWriter id = do
  m <- loadMachine Writer id
  actAsWriter m
  pure m

-- | Subscribes the daemon to the machine's pubsub topic to listen for
-- input requests.
actAsWriter :: IpfsMachine -> Daemon ()
actAsWriter m = do
    env <- ask
    _ <- liftIO $ addHandler (chainSubscription m) (daemonHandler env id onMsg)
    logInfo Normal "Acting as writer" [("machine-id", getMachineId id)]
  where
    id = chainName m
    onMsg = \case
      Submit SubmitInputs{..} -> writeInputs id expressions (Just nonce) >> pure ()
      _ -> pure ()

-- | Write and evaluate inputs in a machine we control.
--
-- Returns the outputs generated by evaluating the inputs.
--
-- Sends out a 'InputsApplied' message over pubsub that includes the
-- outputs and the given nonce.
writeInputs :: MachineId -> [Value] -> Maybe Text -> Daemon [Value]
writeInputs id is nonce = do
    (rs, idx) <- modifyMachine id $ addInputs is write pub
    logInfo Debug "Wrote inputs to IPFS" [ ("machine-id", getMachineId id)
                                         , ("new-input-index", show idx) ]
    pure rs
  where
    write = ipfs id $ writeIpfs id is
    pub results = ipfs id $ publish id (New InputsApplied{..})

-- * Polling

-- | Fetch and apply new inputs for all machines in reader mode.
poll :: Daemon ()
poll = do
    msVar <- asks machines
    ms <- liftIO $ CMap.nonAtomicRead (getChains msVar)
    traverse_ pollMachine ms
  where
    pollMachine :: IpfsMachine -> Daemon ()
    pollMachine m@Chain{chainMode = Reader, ..} = do
      delta <- liftIO $ sinceLastUpdate m
      let (shouldPoll, newPoll) =
            case chainPolling of
              HighFreq more ->
                let more' = more - delta
                in if more' > 0
                   then (True, HighFreq more')
                   else (False, LowFreq)
              -- Low frequency polling is every 10 seconds.
              LowFreq -> (delta > lowFreqPollPeriod, LowFreq)
      when shouldPoll $ do
        logInfo Debug "Polling.." [("machine-id", getMachineId chainName)]
        refreshAsReader chainName >> pure ()
      modifyMachine chainName $ \m' -> pure (m' { chainPolling = newPoll }, () )
    pollMachine _ = pure ()

-- | Returns the amount of time since the last time the machine was
-- updated.
sinceLastUpdate :: IpfsMachine -> IO Milliseconds
sinceLastUpdate m = timeDelta (chainLastUpdated m) <$> Time.getSystemTime
  where
    timeDelta x y =
      case (trunc x, trunc y) of
        (Time.MkSystemTime s n, Time.MkSystemTime s' n') -> 1000 * (s' - s) + fromIntegral (n' - n) `div` 1000000
    trunc = Time.truncateSystemTimeLeapSecond

-- | Polling loop that looks for changes on all loaded reader machines
-- and updates them.
initPolling :: Env -> IO Void
initPolling env = do
  threadDelay (millisToMicros highFreqPollPeriod)
  res <- runDaemon env poll
  -- If polling encounters an error it should log it but continue.
  -- Later we might detect some errors as critical and halt the
  -- daemon.
  case res of
    Left err -> logDaemonError err
    Right _  -> pure ()
  initPolling env

-- * Timings

type Milliseconds = Int64

millisToMicros :: Milliseconds -> Int
millisToMicros n = 1000 * fromIntegral n

-- | High-frequency polling happens once every half-second.
highFreqPollPeriod :: Milliseconds
highFreqPollPeriod = 500

-- | Low-frequency polling happens once every 10 seconds.
lowFreqPollPeriod :: Milliseconds
lowFreqPollPeriod = 10 * 1000

-- | The amount of time a reader will wait for a response message from
-- a writer: 8 seconds.
ackWaitTime :: Milliseconds
ackWaitTime = 8 * 1000

-- | The amount of time a machine does high-frequency polling for
-- before it returns to low-frequency polling: 10 minutes.
highFreq :: Polling
highFreq = HighFreq (10 * 60 * 1000)
