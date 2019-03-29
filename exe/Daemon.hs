-- | The radicle daemon; a long-running background process which
-- materialises the state of remote IPFS machines on the user's PC, and
-- writes to those IPFS machines the user is an owner of.
--
-- See
-- <https://github.com/oscoin/radicle/blob/master/rfcs/0003-radicle-daemon.rst
-- the RFC>.
module Daemon (main) where

import           Protolude hiding
                 ( async
                 , catch
                 , fromStrict
                 , option
                 , poll
                 , tryJust
                 , wait
                 , waitEitherCatchCancel
                 )

import           Control.Exception.Safe hiding (Handler)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time.Clock.System as Time
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy(..))
import qualified Network.Wai.Middleware.Cors as Cors
import           Servant
import           System.IO (BufferMode(..), hSetBuffering)
import           UnliftIO.Async

import qualified Radicle.Daemon.Cli as Cli
import           Radicle.Daemon.Error
import qualified Radicle.Daemon.HttpApi as Api
import           Radicle.Daemon.Ipfs
import           Radicle.Daemon.Logging
import           Radicle.Daemon.MachineConfig
import           Radicle.Daemon.MachineStore
import           Radicle.Daemon.Monad

import qualified Radicle.Ipfs as Ipfs

import           Radicle hiding (DaemonError, Env)
import qualified Radicle.Internal.ConcurrentMap as CMap
import qualified Radicle.Internal.UUID as UUID


-- | Repeatedly tries to connect to the Radicle IPFS daemon API until
-- succesfull. Times out after five seconds and throws 'IpfsDaemonNotReachable'.
waitForIpfsDaemon :: Daemon ()
waitForIpfsDaemon = do
    isOnline <- checkIpfsIsOnline
    if isOnline
    then pure ()
    else do
        logInfo "Waiting for Radicle IPFS daemon" []
        go 5
  where
    go :: Int -> Daemon ()
    go 0 = throw IpfsDaemonNotReachable
    go n = do
        isOnline <- checkIpfsIsOnline
        if isOnline
        then pure ()
        else do
            liftIO $ threadDelay (1000 * 1000)
            go (n - 1)

    checkIpfsIsOnline :: Daemon Bool
    checkIpfsIsOnline = do
        result <- tryJust isIpfsExceptionNoDaemon Ipfs.version
        pure $ case result of
            Left _  -> False
            Right _ -> True

    isIpfsExceptionNoDaemon :: Ipfs.IpfsException -> Maybe ()
    isIpfsExceptionNoDaemon Ipfs.IpfsExceptionNoDaemon = Just ()
    isIpfsExceptionNoDaemon _                          = Nothing

-- We use 'Void' to enforce the use of 'exitFailure'
main :: IO Void
main = do
    hSetBuffering stdout LineBuffering
    Cli.Opts{..} <- Cli.parse
    machineConfigFileLock <- newMVar ()
    machines <- CachedMachines <$> CMap.empty
    envIpfsClient <- Ipfs.newClient
    let env = Env{ logLevel = if debug then LogDebug else LogInfo, ..}
    machineConfig <- readMachineConfigIO machineConfigFileLock machineConfigFile
    initRes <- runDaemon env $ do
        waitForIpfsDaemon
        init machineConfig
    case initRes of
      Left err -> do
        logDaemonError err
        exitFailure
      Right _ -> do
        polling <- async $ initPolling env
        let app = corsWithContentType $ serve Api.daemonApi (server env)
        runDaemon env $ logInfo "Start listening" [("port", show port)]
        serv <- async $ run port app
        exc <- waitEitherCatchCancel polling serv
        case exc of
          Left (Left err) -> do
            logError' "Polling failed with an exception" [("error", toS (displayException err))]
            exitFailure
          Left (Right void') -> absurd void'
          Right (Left err) -> do
            logError' "Server failed with an exception" [("error", toS (displayException err))]
            exitFailure
          Right (Right ()) -> do
            logError' "Server stopped (this should not happen)" []
            exitFailure
  where
    -- | Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType = Cors.cors (const $ Just policy)
      where
        policy = Cors.simpleCorsResourcePolicy
                   { corsRequestHeaders = ["Content-Type"] }

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
          _ -> err500

-- | Initiate machines according to follow file.
init :: MachinesConfig -> Daemon ()
init follows = pooledForConcurrentlyN_ 5 (Map.toList follows) initMachine
  where
    initMachine (id, ConfigReader) = do
        result <- tryError $ initAsReader id
        case result of
            Right m -> insertMachine m
            Left err  -> do
                let (msg, infos) = displayError err
                logError' "Could not initiate reader-mode machine on startup" $ ("init-error", msg) : infos
                insertUninitializedReader id
    initMachine (id, ConfigWriter idx) = do
        machineIpfs id $ ipnsPublish id idx
        m <- loadMachine Writer id
        insertMachine m
        actAsWriter m

    tryError f = (Right <$> f) `catch` (pure . Left)

-- * Endpoints

-- | Create a new IPFS machine and initialise the daemon as as the
-- /writer/.
newMachine :: Daemon Api.NewResponse
newMachine = do
    id <- wrapException CouldNotCreateMachine createMachine
    sub <- initDaemonSubscription id
    logInfo "Created new IPFS machine" [("machine-id", getMachineId id)]
    m <- emptyMachine id Writer sub
    insertMachine m
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
  m <- ensureMachineLoaded id
  bumpPolling id
  case fst <$> runIdentity $ runLang (machineState m) $ eval v of
    Left err -> throw $ MachineError id (InvalidInput err)
    Right rv -> do
      logInfo "Handled query" [("machine-id", getMachineId id)]
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
    Machine{machineMode, machineSubscription} <- ensureMachineLoaded id
    case machineMode of
        Reader -> requestInput machineSubscription
        Writer -> do
            results <- writeInputs "http" id expressions Nothing
            pure Api.SendResponse{..}
  where
    matchMessage nonce = \case
        (New InputsApplied{results, nonce = Just nonce'}) | nonce' == nonce -> Just results
        _ -> Nothing
    requestInput sub = do
      nonce <- liftIO $ UUID.uuid
      asyncMsg <- liftIO $ async $ subscribeOne sub ackWaitTime (matchMessage nonce)
      machineIpfs id $ publish id (Submit SubmitInputs{..})
      logInfo "Sent input request to writer"
              [ ("machine-id", getMachineId id)
              , ("expressions", valuesForLog expressions)
              , ("nonce", nonce)
              ]
      msg_ <- machineIpfs id $ wait asyncMsg
      case msg_ of
        Just results -> do
          logInfo "Writer accepted input request"
                  [ ("machine-id", getMachineId id)
                  , ("results", valuesForLog results)
                  , ("nonce", nonce)
                  ]
          bumpPolling id
          pure Api.SendResponse{..}
        Nothing -> throw $ MachineError id AckTimeout


-- | Given an 'MachineId', makes sure the machine is loaded and
-- fetch the latest inputs.
--
-- If the machine is not yet loaded into memory we fetch its inputs, load it
-- into memory and start following its changes.
ensureMachineLoaded :: MachineId -> Daemon Machine
ensureMachineLoaded id =
    modifyMachine' id $ \case
        Nothing -> do
            m <- initAsReader id
            writeMachineConfig
            pure (Just (Cached m), m)
        Just (UninitialisedReader _) -> do
            m <- initAsReader id
            pure (Just (Cached m), m)
        Just (Cached m) -> do
            m' <- pullInputs m
            pure (Just (Cached m'), m')

-- | Creates a IPFS pusbsub subscription for the given machine. This
-- wraps 'initSubscription' and logs all message parsing errors.
initDaemonSubscription :: MachineId -> Daemon TopicSubscription
initDaemonSubscription id =
    machineIpfs id $ initSubscription id logNonDecodableMsg
  where
    logNonDecodableMsg bad =
        logError' "Non-decodable message on machine's pubsub topic"
            [("machine-id", getMachineId id), ("message", bad)]

-- | Load a machine by pulling all data from IPFS
loadMachine :: ReaderOrWriter -> MachineId -> Daemon Machine
loadMachine mode id = do
  (idx, is) <- machineIpfs id $ machineInputsFrom id Nothing
  sub <- initDaemonSubscription id
  m <- emptyMachine id mode sub
  addInputs is idx m

-- | Runs some inputs over the state of a cached machine. Returns the new state
-- that would result, and the outputs. Throws an error if the inputs are
-- invalid.
runInputs :: Machine -> [Value] -> Daemon (Bindings (PrimFns Identity), [Value])
runInputs m inputs = do
  let (result, newState) = runIdentity $ runLang (machineState m) $ traverse eval inputs
  case result of
    Left err      -> throw $ MachineError (machineId m) (InvalidInput err)
    Right outputs -> pure (newState, outputs)

-- | Modify the state and index of a cached machine.
updateState :: Bindings (PrimFns Identity) -> MachineEntryIndex -> Machine -> Daemon Machine
updateState newState index m = do
  t <- liftIO $ Time.getSystemTime
  let m' = m { machineState = newState
             , machineLastIndex = index
             , machineLastUpdated = t
             }
  pure m'

-- | Add inputs to a machine and update to a new index. Checks that the new
-- inputs are valid.
addInputs :: [Value] -> MachineEntryIndex -> Machine -> Daemon Machine
addInputs is idx m = do
  (newState, _) <- runInputs m is
  updateState newState idx m

machineIpfs :: MachineId -> Daemon a -> Daemon a
machineIpfs id io = wrapException (MachineError id . IpfsError) io

-- | Do some high-freq polling for a while.
bumpPolling :: MachineId -> Daemon ()
bumpPolling id = do
  modifyMachine id $ \m -> pure (m { machinePolling = highFreq }, () )
  logDebug "Reset to high-frequency polling" [("machine-id", getMachineId id)]

-- ** Reader

-- | Load a machine in /reader mode/ and return it.
--
-- Loads the machines input log from IPFS and listens for machine
-- updates on pubsub.
initAsReader :: MachineId -> Daemon Machine
initAsReader id = do
    m <- loadMachine Reader id
    installMachineMessageHandler m onMsg
    logInfo "Following as reader" [ ("machine-id", getMachineId id)
                                  , ("current-input-index", show (machineLastIndex m)) ]
    pure m
  where
    onMsg :: Message -> Daemon ()
    onMsg = \case
      New InputsApplied{..} -> do
        pullStoreInputs id
        -- TODO(james): decide if this should bump polling. If this is
        -- a very active chain this will mean the polling is always
        -- high-frequency.
        bumpPolling id
      _ -> pure ()

-- | Updates the machine in the store by calling 'pullInputs'.
pullStoreInputs :: MachineId -> Daemon ()
pullStoreInputs id = modifyMachine id $ \m -> do
    m' <- pullInputs m
    pure (m', ())

-- | Fetch latest inputs from the network and apply them. Returns the
-- updated machine.
--
-- This is a no-op for machines we own.
pullInputs :: Machine -> Daemon Machine
pullInputs m =
    case machineMode m of
        Writer -> pure m
        Reader -> do
            let currentIdx = machineLastIndex m
            (newIdx, is) <- machineIpfs (machineId m) $ machineInputsFrom (machineId m) (Just currentIdx)
            if currentIdx == newIdx
            then pure m
            else do
                m' <- addInputs is newIdx m
                logDebug "Updated reader" [mid, ("n", show (length is)), ("input-index", show newIdx)]
                pure m'

  where
    mid = ("machine-id", getMachineId (machineId m))

-- ** Writer

-- | Subscribes the daemon to the machine's pubsub topic to listen for
-- input requests.
actAsWriter :: Machine -> Daemon ()
actAsWriter m = do
    installMachineMessageHandler m onMsg
    logInfo "Acting as writer" [("machine-id", getMachineId id)]
  where
    id = machineId m
    onMsg = \case
      Submit SubmitInputs{..} -> do
        logDebug "Writer received write-request"
                 [ ("machine-id", getMachineId id)
                 , ("nonce", nonce)
                 , ("expressions", valuesForLog expressions)
                 ]
        writeInputs "pubsub" id expressions (Just nonce) >> pure ()
      _ -> pure ()

-- | Installs a handler for messages sent on the machine's IPFS pubsub channel.
--
-- If the handler produces an error it is logged.
installMachineMessageHandler :: Machine -> (Message -> Daemon ()) -> Daemon ()
installMachineMessageHandler m handleMessage = do
    env <- ask
    void $ liftIO $ addHandler (machineSubscription m) $ \msg -> do
        result <- runDaemon env (handleMessage msg)
        case result of
            Left err -> logDaemonError err
            Right () -> pure ()

-- | Write and evaluate inputs in a machine we control.
--
-- Returns the outputs generated by evaluating the inputs.
--
-- Sends out a 'InputsApplied' message over pubsub that includes the
-- outputs and the given nonce.
--
-- @source@ is the source of the inputs (either HTTP or pubsub) and is
-- only used for logging.
writeInputs :: Text -> MachineId -> [Value] -> Maybe Text -> Daemon [Value]
writeInputs source id inputs nonce = do
    (rs, idx) <- modifyMachine id $ \machine -> do
        -- We check that the new inputs are valid before writing them to IPFS.
        (newState, results) <- runInputs machine inputs
        newIndex <- machineIpfs id $ writeIpfs id inputs
        machine' <- updateState newState newIndex machine
        machineIpfs id $ publish id (New InputsApplied{results, nonce})
        pure (machine', (results, machineLastIndex machine'))
    writeMachineConfig
    logInfo "Updated machine"
        [ ("machine-id", getMachineId id)
        , ("new-input-index", show idx)
        , ("source", source)
        ]
    pure rs

valuesForLog :: [Value] -> Text
valuesForLog = T.intercalate "," . (renderCompactPretty <$>)

-- * Polling

-- | Fetch and apply new inputs for all machines in reader mode.
pollMachines :: Daemon ()
pollMachines = traverseMachines pollMachine
  where
    pollMachine :: MachineId -> CachedMachine -> Daemon ()
    pollMachine _ = \case
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
        when shouldPoll $ pullStoreInputs machineId
        modifyMachine machineId $ \m' -> pure (m' { machinePolling = newPoll }, ())
      -- Don't update owned and unavailable machines.
      _ -> pure ()

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
  res <- runDaemon env pollMachines
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
-- a writer: 12 seconds.
ackWaitTime :: Milliseconds
ackWaitTime = 12 * 1000
