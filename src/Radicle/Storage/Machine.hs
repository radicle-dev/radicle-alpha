{-# LANGUAGE StrictData #-}

module Radicle.Storage.Machine
    ( Settings (..)
    , defaultSettings

    , MEnv
    , InitialiserError (..)

    , TaskResult (resultGet, resultPeek)
    , hoistTaskResult
    , mapTaskResultError

    , runMachine
    , shutdownMachine
    , loadMachine
    , updateMachine
    , getMachine
    )
where

import           Protolude hiding
                 (async, atomically, evaluate, link, race, withAsync)

import           Control.Exception.Safe (MonadThrow, throw)
import           UnliftIO hiding (timeout)

import           Radicle
import           Radicle.Internal.Core (emptyBindings)
import           Radicle.Storage.Tip
import           Radicle.Storage.Types


data Settings = Settings
    { sEvalTimeoutMicros    :: Word64
    , sBackendTimeoutMicros :: Word64
    , sEvalQueueSize        :: Word16
    }

defaultSettings :: Settings
defaultSettings = Settings
    { sEvalTimeoutMicros    = 3 * 1_000_000
    , sBackendTimeoutMicros = 5 * 1_000_000
    , sEvalQueueSize        = 42
    }

data EvalError e
    = EvalError (LangError Value)
    | EvalTimeout
    | BackendError e
    | BackendTimeout
    | SaveTipError SomeException

data Task e = Task
    { taskInputs :: Inputs
    , taskResult :: TMVar (Either (EvalError e) Outputs)
    }

data TaskResult e m = TaskResult
    { resultGet  :: m (Either (EvalError e) Outputs)
    , resultPeek :: m (Maybe (Either (EvalError e) Outputs))
    }

newTaskResult
    :: MonadIO m
    => TMVar (Either (EvalError e) Outputs)
    -> TaskResult e m
newTaskResult ref = TaskResult
    { resultGet  = atomically $ takeTMVar    ref
    , resultPeek = atomically $ tryReadTMVar ref
    }

hoistTaskResult
    :: (forall a. m a -> n a)
    -> TaskResult e m
    -> TaskResult e n
hoistTaskResult f t = t
    { resultGet  = f $ resultGet  t
    , resultPeek = f $ resultPeek t
    }

mapTaskResultError
    :: Functor m
    => (e -> e')
    -> TaskResult e  m
    -> TaskResult e' m
mapTaskResultError f t = t
    { resultGet  = first (mapEvalError f) <$> resultGet t
    , resultPeek = fmap (first (mapEvalError f)) <$> resultPeek t
    }

mapEvalError :: (e -> e') -> EvalError e -> EvalError e'
mapEvalError f = \case
    EvalError    e -> EvalError e
    EvalTimeout    -> EvalTimeout
    BackendError e -> BackendError (f e)
    BackendTimeout -> BackendTimeout
    SaveTipError e -> SaveTipError e

data MEnv e mid idx = MEnv
    { envSettings :: Settings
    , envMachine  :: TVar (Machine mid idx)
    , envQueue    :: TBQueue (Task e)
    , envEval     :: Async Void
    , envAlive    :: IORef ()
    }

data InitialiserError mid
    = ExceptionInInitialiserError SomeException -- mwaha
    | UnknownMachine mid
    deriving Show

instance (Typeable mid, Show mid) => Exception (InitialiserError mid)

-- | Run a stateful 'Machine' identified by @mid@.
--
-- If 'Just idx' is given, the machine is loaded from the 'Backend', otherwise
-- the internal state is initialised with an empty machine.
--
runMachine
    :: (MonadUnliftIO m, MonadThrow m, Exception e)
    => Settings
    -> Backend e mid idx m
    -> TipStore e mid idx m
    -> mid
    -> Maybe idx
    -> ReaderOrWriter
    -> m (Either (InitialiserError mid) (MEnv e mid idx))
runMachine settings backend tips mid idx mode = runExceptT $ do
    machine <-
        case idx of
            Nothing  -> pure emptyMachine
            Just tip -> do
                machine <- tryAnyE $ loadMachine backend mid tip mode
                case machine of
                    Nothing -> throwE $ UnknownMachine mid
                    Just  m -> pure m

    tryAnyE $ initMachine settings backend tips machine
  where
    tryAnyE = withExceptT ExceptionInInitialiserError . ExceptT . tryAny

    emptyMachine = Machine
        { machineId    = mid
        , machineTip   = Nothing
        , machineState = emptyBindings
        , machineMode  = mode
        }

shutdownMachine :: MonadIO m => MEnv e mid idx -> m ()
shutdownMachine MEnv { envEval } = uninterruptibleCancel envEval

-- | Load the 'Machine' @mid@ at index @idx@ from the 'Backend'.
--
-- Returns 'Nothing' if the machine is not known by the 'Backend', otherwise
-- 'Just' the 'Machine'. Exceptions thrown by the 'Backend' are rethrown.
--
-- The 'Inputs' returned by the 'Backend' are evaluated against the
-- 'emptyBindings'. 'panic's if evaluation returns an error.
--
loadMachine
    :: (MonadUnliftIO m, MonadThrow m, Exception e)
    => Backend e mid idx m
    -> mid
    -> idx
    -> ReaderOrWriter
    -> m (Maybe (Machine mid idx))
loadMachine backend mid idx mode = do
    -- TODO(kim): timeout
    bindings <-
        either throw pure
            =<< backendFoldUpto backend mid idx f emptyBindings
    pure . Just $ Machine
        { machineId    = mid
        , machineTip   = Just idx
        , machineState = bindings
        , machineMode  = mode
        }
  where
    f _ inputs bindings =
        let
            (result, newBindings) =
                runIdentity . runLang bindings $ traverse eval inputs
         in
            case result of
                Left  e -> panic $ "Invalid inputs from backend: " <> show e
                Right _ -> newBindings

-- | Update the 'Machine' with new 'Inputs'.
--
-- This merely enqueues the 'Inputs' to be evaluated and applied later. The
-- returned 'TaskResult' can be used to block on the result.
--
updateMachine
    :: MonadUnliftIO m
    => MEnv e mid idx
    -> Inputs
    -> m (TaskResult e m)
updateMachine env inputs = do
    res <- newEmptyTMVarIO
    atomically $ writeTBQueue (envQueue env) (Task inputs res)
    pure $ newTaskResult res

getMachine :: MEnv e mid idx -> STM (Machine mid idx)
getMachine MEnv { envMachine } = readTVar envMachine

-- Internal --------------------------------------------------------------------

initMachine
    :: MonadUnliftIO m
    => Settings
    -> Backend e mid idx m
    -> TipStore e mid idx m
    -> Machine mid idx
    -> m (MEnv e mid idx)
initMachine settings backend tips machine = do
    machineRef <- newTVarIO machine
    queue      <- newTBQueueIO (fromIntegral $ sEvalQueueSize settings)
    eval'      <- async $ evalThread settings backend tips machineRef queue
    -- kill eval thread when 'MEnv' goes out of scope
    alive      <- newIORef ()
    void $ mkWeakIORef alive (uninterruptibleCancel eval')
    -- link the eval thread to the current one: there is no way to recover
    link eval'

    pure MEnv
        { envSettings = settings
        , envMachine  = machineRef
        , envQueue    = queue
        , envEval     = eval'
        , envAlive    = alive
        }

evalThread
    :: MonadUnliftIO m
    => Settings
    -> Backend e mid idx m
    -> TipStore e mid idx m
    -> TVar (Machine mid idx)
    -> TBQueue (Task e)
    -> m Void
evalThread settings backend tips machineRef queue =
    forever $ do
        task <- atomically $ readTBQueue queue
        res  <- runTask settings backend tips machineRef task
        atomically . void $ swapTMVar (taskResult task) res

runTask
    :: MonadUnliftIO m
    => Settings
    -> Backend e mid idx m
    -> TipStore e mid idx m
    -> TVar (Machine mid idx)
    -> Task e
    -> m (Either (EvalError e) Outputs)
runTask settings backend tips machineRef task = runExceptT $ do
    machine <- ExceptT $ pure <$> readTVarIO machineRef

    (outputs, newState) <-
        withExceptT (const EvalTimeout) . ExceptT
            . timeout (sEvalTimeoutMicros settings)
            . liftIO $ evalInputs
                (machineState machine) (expressions (taskInputs task))

    case outputs of
        Left  e    -> throwE $ EvalError e
        Right outs -> do
            backendResult <-
                withExceptT (const BackendTimeout) . ExceptT
                    . timeout (sBackendTimeoutMicros settings)
                    $ backendPut backend (machineId machine) (taskInputs task)

            case backendResult of
                Left  e -> throwE $ BackendError e
                Right (PutResponse idx nonce confirm) -> do
                    withExceptT SaveTipError . ExceptT . tryAny $
                        saveTip tips
                                (machineId machine)
                                idx
                                (machineMode machine)
                    ExceptT $ do
                        atomically . modifyTVar' machineRef $ \m ->
                            m { machineState = newState
                              , machineTip   = Just idx
                              }
                        confirm $> Right Expressions
                            { expressions = outs
                            , nonce       = nonce
                            }

evalInputs
    :: Bindings (PrimFns Identity)
    -> [Value]
    -> IO (Either (LangError Value) [Value], Bindings (PrimFns Identity))
evalInputs bindings inputs = evaluate $
    runIdentity . runLang bindings $ traverse eval inputs

timeout :: MonadUnliftIO m => Word64 -> m a -> m (Either () a)
timeout t m = race (liftIO $ threadDelay (fromIntegral t)) m
