-- | The radicle-daemon; a long-running background process which
-- materialises the state of remote IPFS machines on the user's PC, and
-- writes to those IPFS machines the user is an owner of.
--
-- See
-- <https://github.com/oscoin/radicle/blob/master/rfcs/0003-radicle-daemon.rst
-- the RFC>.
module Daemon (main) where

import           Protolude hiding (fromStrict, option, poll)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time.Clock.System as Time
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
import           Servant
import           System.IO (BufferMode(..), hSetBuffering)

import           Radicle.Daemon.Common hiding (logInfo)
import qualified Radicle.Daemon.Common as Common
import qualified Radicle.Daemon.HttpApi as Api
import           Radicle.Daemon.Ipfs
import           Radicle.Daemon.MachineConfig
import           Radicle.Daemon.Monad

import qualified Radicle.Ipfs as Ipfs

import           Radicle hiding (DaemonError, Env)
import qualified Radicle.Internal.CLI as Local
import qualified Radicle.Internal.ConcurrentMap as CMap
import qualified Radicle.Internal.UUID as UUID

-- * Types

logDaemonError :: MonadIO m => Error -> m ()
logDaemonError (displayError -> (m, xs)) = logErr m xs

-- * Main

data Opts = Opts
  { port              :: Int
  , machineConfigFile :: FilePath
  , debug             :: Bool
  }

cliInfo :: IO (ParserInfo Opts)
cliInfo = do
    defaultMachineConfigFile <- Local.getRadicleFile "machines.json"
    pure $
        info (opts defaultMachineConfigFile <**> helper)
            ( fullDesc
           <> progDesc "Run the radicle daemon"
           <> header "radicle-daemon"
            )

opts :: FilePath -> Parser Opts
opts defaultMachineConfigFile = do
    port <-
        option auto
        ( long "port"
       <> help "daemon port"
       <> metavar "PORT"
       <> showDefault
       <> value 8909
        )
    machineConfigFile <-
        strOption
        ( long "machine-config"
       <> help "Where to store configuration for known machines"
       <> metavar "FILE"
       <> showDefault
       <> value defaultMachineConfigFile
        )
    debug <-
        switch
        ( long "debug"
       <> help "enable debug logging"
       <> showDefault
        )
    pure $ Opts{..}

-- We use 'Void' to enforce the use of 'exitFailure'
main :: IO Void
main = do
    hSetBuffering stdout LineBuffering
    Opts{..} <- execParser =<< cliInfo
    machineConfigFileLock <- newMVar ()
    machines <- CachedMachines <$> CMap.empty
    let env = Env{ logLevel = if debug then Debug else Normal, ..}
    machineConfig <- readMachineConfigIO machineConfigFileLock machineConfigFile
    initRes <- runDaemon env (init machineConfig)
    case initRes of
      Left err -> do
        logDaemonError err
        exitFailure
      Right _ -> do
        polling <- async $ initPolling env
        let app = serve Api.daemonApi (server env)
        Common.logInfo "Start listening" [("port", show port)]
        serv <- async $ run port app
        exc <- waitEitherCatchCancel polling serv
        case exc of
          Left (Left err) -> do
            logErr "Polling failed with an exception" [("error", toS (displayException err))]
            exitFailure
          Left (Right void') -> absurd void'
          Right (Left err) -> do
            logErr "Server failed with an exception" [("error", toS (displayException err))]
            exitFailure
          Right (Right ()) -> do
            logErr "Server stopped (this should not happen)" []
            exitFailure

server :: Env -> Server Api.DaemonApi
server env = hoistServer Api.daemonApi nt daemonServer
  where
    daemonServer :: ServerT Api.DaemonApi Daemon
    daemonServer = newMachine :<|> query :<|> send :<|> pure Api.swagger

    nt :: Daemon a -> Handler a
    nt d = do
      x_ <- liftIO $ runDaemon env d
      case x_ of
        Left err -> do
            logDaemonError err
            throwError (toServantErr err)
        Right x  -> pure x

    toServantErr err = errCode { errBody = toS bod }
      where
        (msg, infos) = displayError err
        bod = msg <> ":" <> T.concat ["\n  " <> k <> ": " <> v | (k,v) <- infos]
        errCode = case err of
          MachineError _ e -> case e of
            InvalidInput _ -> err400
            AckTimeout     -> err504
            _              -> err500
          CouldNotCreateMachine _ -> err500

-- * Init

-- | Initiate machines according to follow file.
init :: MachineConfig -> Daemon ()
init follows = traverse_ initMachine (Map.toList follows)
  where
    initMachine (id, Reader) = initReaderNoFail id
    initMachine (id, Writer) = initAsWriter id $> ()

-- * Endpoints

-- | Create a new IPFS machine and initialise the daemon as as the
-- /writer/.
newMachine :: Daemon Api.NewResponse
newMachine = do
    id <- createMachine `ipfs` CouldNotCreateMachine
    sub <- machineIpfs id $ initSubscription id
    logInfo Normal "Created new IPFS machine" [("machine-id", getMachineId id)]
    m <- liftIO $ emptyMachine id Writer sub
    insertNewMachine id (Cached m)
    actAsWriter m
    writeMachineConfig
    pure (Api.NewResponse id)

-- | Evaluate an expression against a cached machine. The resulting
-- state is always discarded, and the expression is never sent to the
-- writer. Only used for local queries.
--
-- Hitting this endpoint will turn on high-frequency polling for a
-- fixed amount of time.
query :: MachineId -> Api.QueryRequest -> Daemon Api.QueryResponse
query id (Api.QueryRequest v) = do
  m <- checkMachineLoaded id
  bumpPolling id
  case fst <$> runIdentity $ runLang (machineState m) $ eval v of
    Left err -> throwError $ MachineError id (InvalidInput err)
    Right rv -> do
      logInfo Normal "Handled query" [("machine-id", getMachineId id)]
      pure (Api.QueryResponse rv)

-- | Write a new expression to an IPFS machine.
--
-- - If the daemon is the writer for the machine, it will write the
--   new inputs to IPFS and then send out a notification on the
--   machine's pubsub topic.
--
-- - If the daemon is a reader for the machine, it will request the
--   machine's writer daemon to perform the write, and wait for an
--   ack.
send :: MachineId -> Api.SendRequest -> Daemon Api.SendResponse
send id (Api.SendRequest expressions) = do
  cm_ <- lookupMachine id
  case cm_ of
    Nothing -> do
      m <- newReader id
      requestInput (machineSubscription m)
    Just (UninitialisedReader _) -> do
      m <- initAsReader id
      requestInput (machineSubscription m)
    Just (Cached Machine{ machineMode = Reader, ..}) -> requestInput machineSubscription
    Just (Cached Machine{ machineMode = Writer }) -> do
      logInfo Normal "Applying input"
        [ ("machine-id", getMachineId id)
        ]
      results <- writeInputs id expressions Nothing
      pure Api.SendResponse{..}
  where
    requestInput sub = do
      nonce <- liftIO $ UUID.uuid
      let isResponse = \case
            New InputsApplied{ nonce = Just nonce' } | nonce' == nonce -> True
            _ -> False
      asyncMsg <- liftIO $ async $ subscribeOne sub ackWaitTime isResponse (logNonDecodableMsg id)
      machineIpfs id $ publish id (Submit SubmitInputs{..})
      logInfo Debug
              "Sent input request to writer"
              [ ("machine-id", getMachineId id)
              , ("expressions", prValues expressions) ]
      msg_ <- machineIpfs id $ wait asyncMsg
      case msg_ of
        Just (New InputsApplied{results}) -> do
          logInfo Normal
                 "Writer accepted input request"
                 [ ("machine-id", getMachineId id)
                 , ("results", prValues results)
                 ]
          bumpPolling id
          pure Api.SendResponse{..}
        Just _ -> throwError $ MachineError id (DaemonError "Didn't filter machine topic messages correctly.")
        Nothing -> throwError $ MachineError id AckTimeout


    prValues = T.intercalate "," . (renderCompactPretty <$>)

-- * Helpers

logInfo :: LogLevel -> Text -> [(Text,Text)] -> Daemon ()
logInfo l msg infos = do
  l' <- asks logLevel
  if l <= l'
    then Common.logInfo msg infos
    else pure ()


lookupMachine :: MachineId -> Daemon (Maybe CachedMachine)
lookupMachine id = do
  msCMap <- asks machines
  liftIO $ CMap.lookup id (getMachines msCMap)

-- | Given an 'MachineId', makes sure the machine is in the cache and
-- updated.
checkMachineLoaded :: MachineId -> Daemon Machine
checkMachineLoaded id = do
  m_ <- lookupMachine id
  case m_ of
    Nothing -> do
      -- In this case we have not seen the machine before so we act as
      -- a reader.
      newReader id
    Just (UninitialisedReader _) ->
      -- We try to initialise the reader again.
      initAsReader id
    Just (Cached m) -> case machineMode m of
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

-- | Turns a 'Daemon' subscription handler into an IO one. Errors which are
-- encountered in a subscription handler are just logged.
daemonHandler :: Env -> MachineId -> (Message -> Daemon ()) -> Either Text Message -> IO ()
daemonHandler _ id _ (Left bad) = logNonDecodableMsg id bad
daemonHandler env _ h (Right msg) = do
  x_ <- runDaemon env (h msg)
  case x_ of
    Left err -> logDaemonError err
    Right _  -> pure ()

-- | Loads a machine fresh from IPFS.
loadMachine :: ReaderOrWriter -> MachineId -> Daemon Machine
loadMachine mode id = do
  (idx, is) <- machineIpfs id $ machineInputsFrom id Nothing
  sub <- machineIpfs id $ initSubscription id
  m <- liftIO $ emptyMachine id mode sub
  (m', _) <- addInputs is (pure idx) (const (pure ())) m
  insertNewMachine id (Cached m')
  pure m'

-- | Add inputs to a cached machine.
addInputs
  :: [Value]
  -- ^ Inputs to add.
  -> Daemon MachineEntryIndex
  -- ^ Determine the new index.
  -> ([Value] -> Daemon ())
  -- ^ Performed after the chain is cached.
  -> Machine
  -> Daemon (Machine, ([Value], MachineEntryIndex))
  -- ^ Returns the updated machine, the results and the new index.
addInputs is getIdx after m =
  case advanceChain m is of
    Left err -> throwError $ MachineError (machineId m) (InvalidInput err)
    Right (rs, newState) -> do
      idx <- getIdx
      t <- liftIO $ Time.getSystemTime
      let m' = m { machineState = newState
                 , machineLastIndex = Just idx
                 , machineLastUpdated = t
                 }
      after rs
      pure (m', (rs, idx))

-- | Run some IPFS IO.
ipfs :: IO a -> (Ipfs.IpfsException -> Error) -> Daemon a
ipfs io err = do
  res <- liftIO $ (Right <$> io) `catch` \(e :: Ipfs.IpfsException) -> pure (Left e)
  case res of
    Left e  -> throwError (err e)
    Right x -> pure x

-- | Run some IPFS IO related to a specific machine.
machineIpfs :: MachineId -> IO a -> Daemon a
machineIpfs id io = ipfs io (MachineError id . IpfsError)

-- | Do some high-freq polling for a while.
bumpPolling :: MachineId -> Daemon ()
bumpPolling id = do
  modifyMachine id $ \m -> pure (m { machinePolling = highFreq }, () )
  logInfo Debug "Reset to high-frequency polling" [("machine-id", getMachineId id)]

-- | Insert a new machine into the cache.
insertNewMachine :: MachineId -> CachedMachine -> Daemon ()
insertNewMachine id m = do
    msCMap <- asks machines
    liftIO $ CMap.insert id m (getMachines msCMap)

-- | Modify a machine that is already in the cache. Errors if the
-- machine isn't in the cache already.
modifyMachine :: forall a. MachineId -> (Machine -> Daemon (Machine, a)) -> Daemon a
modifyMachine id f = do
    env <- ask
    res <- liftIO $ CMap.modifyExistingValue id (getMachines (machines env)) (modCached env)
    case res of
      Nothing         -> throwError (MachineError id MachineNotCached)
      Just (Left err) -> throwError err
      Just (Right y)  -> pure y
  where
    modCached :: Env -> CachedMachine -> IO (CachedMachine, Either Error a)
    modCached _ u@(UninitialisedReader _) = pure (u, Left (MachineError id MachineNotCached))
    modCached env (Cached m) = do
      x <- runDaemon env (f m)
      pure $ case x of
        Left err      -> (Cached m, Left err)
        Right (m', y) -> (Cached m', Right y)

emptyMachine :: MachineId -> ReaderOrWriter -> TopicSubscription -> IO Machine
emptyMachine id mode sub = do
  t <- Time.getSystemTime
  pure Machine{ machineId = id
              , machineState = pureEnv
              , machineLastIndex = Nothing
              , machineMode = mode
              , machineSubscription = sub
              , machineLastUpdated = t
              , machinePolling = highFreq
              }

-- ** Reader

-- | Load a machine in /reader mode/ and return it.
--
-- Loads the machines input log from IPFS and listens for machine
-- updates on pubsub.
--
-- The machine is added to the deamons machine cache.
initAsReader :: MachineId -> Daemon Machine
initAsReader id = do
    m <- loadMachine Reader id
    env <- ask
    _ <- liftIO $ addHandler (machineSubscription m) (daemonHandler env id onMsg)
    logInfo Normal "Following as reader" [ ("machine-id", getMachineId id)
                                         , ("current-input-index", show (machineLastIndex m)) ]
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

-- | Initiate a reader the daemon wasn't following before (adds it to the
-- follow-file).
newReader :: MachineId -> Daemon Machine
newReader id = do
  m <- initAsReader id
  writeMachineConfig
  pure m

-- | Try to initiate a reader, but don't fail in case of errors; just log a
-- message.
initReaderNoFail :: MachineId -> Daemon ()
initReaderNoFail id = catchError (initAsReader id $> ()) $ \err -> do
  let (msg, infos) = displayError err
  logErr "Could not initiate reader-mode machine on startup" $ ("init-error", msg) : infos
  t <- liftIO Time.getSystemTime
  insertNewMachine id (UninitialisedReader t)

-- | Updates a (already initialised) machine in the cache with new inputs pulled
-- from IPFS.
refreshAsReader :: MachineId -> Daemon Machine
refreshAsReader id = modifyMachine id refresh
  where
    refresh m = do
      let currentIdx = machineLastIndex m
      (newIdx, is) <- machineIpfs id $ machineInputsFrom (machineId m) currentIdx
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
    mid = ("machine-id", getMachineId id)

-- ** Writer

initAsWriter :: MachineId -> Daemon Machine
initAsWriter id = do
  m <- loadMachine Writer id
  actAsWriter m
  pure m

-- | Subscribes the daemon to the machine's pubsub topic to listen for
-- input requests.
actAsWriter :: Machine -> Daemon ()
actAsWriter m = do
    env <- ask
    _ <- liftIO $ addHandler (machineSubscription m) (daemonHandler env id onMsg)
    logInfo Normal "Acting as writer" [("machine-id", getMachineId id)]
  where
    id = machineId m
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
    (rs, idx) <- modifyMachine id (addInputs is write pub)
    logInfo Debug "Wrote inputs to IPFS" [ ("machine-id", getMachineId id)
                                         , ("new-input-index", show idx) ]
    pure rs
  where
    write = machineIpfs id $ writeIpfs id is
    pub results = machineIpfs id $ publish id (New InputsApplied{..})

-- * Polling

-- | Fetch and apply new inputs for all machines in reader mode.
poll :: Daemon ()
poll = do
    msVar <- asks machines
    ms <- liftIO $ CMap.nonAtomicRead (getMachines msVar)
    _ <- Map.traverseWithKey pollMachine ms
    pure ()
  where
    pollMachine :: MachineId -> CachedMachine -> Daemon ()
    pollMachine id = \case
      Cached m@Machine{ machineMode = Reader, .. } -> do
        delta <- liftIO $ sinceLastUpdate m
        let (shouldPoll, newPoll) =
              case machinePolling of
                HighFreq more ->
                  let more' = more - delta
                  in if more' > 0
                     then (True, HighFreq more')
                     else (False, LowFreq)
                -- Low frequency polling is every 10 seconds.
                LowFreq -> (delta > lowFreqPollPeriod, LowFreq)
        when shouldPoll $ do
          logInfo Debug "Polling.." [("machine-id", getMachineId machineId)]
          refreshAsReader machineId >> pure ()
        modifyMachine machineId $ \m' -> pure (m' { machinePolling = newPoll }, ())
      UninitialisedReader t -> do
        delta <- liftIO $ timeSince t
        when (delta > lowFreqPollPeriod) $ initReaderNoFail id $> ()
      _ -> pure () -- Writers don't require any polling.

-- | Returns the amount of time since the last time the machine was
-- updated.
sinceLastUpdate :: Machine -> IO Milliseconds
sinceLastUpdate = timeSince . machineLastUpdated

timeSince :: Time.SystemTime -> IO Milliseconds
timeSince x = timeDelta x <$> Time.getSystemTime
  where
    timeDelta a b =
        case (trunc a, trunc b) of
          (Time.MkSystemTime s n, Time.MkSystemTime s' n') -> 1000 * (s' - s) + fromIntegral (n' - n) `div` 1000000
      where
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
