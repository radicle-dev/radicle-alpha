-- | Client for the daemons machine API.
--
-- Also provides primitive functions to bind the client into the Radicle
-- interpreter with @daemonClientPrimFns@.
module Radicle.Daemon.Client
    ( daemonClientPrimFns
    , newMachine
    , query
    , send
    , MachineId(..)
    ) where

import           Protolude hiding (TypeError)

import           Control.Monad.Except
import           GHC.Exts (fromList)
import qualified Network.HTTP.Client as HttpClient
import           Servant.Client hiding (Client)

import           Radicle
import           Radicle.Daemon.HttpApi
import qualified Radicle.Internal.PrimFns as PrimFns


-- | Constraints for all client functions
type Client m = (MonadIO m, MonadError ServantError m)

-- | Send an input to a machine to be evaluated and applied. Returns the results
-- of evaluting the inputs.
send :: Client m => MachineId -> Seq Value -> m [Value]
send machineId inputs = do
    SendResponse {..} <- runClient $ client machineSendEndpoint machineId (SendRequest $ toList inputs)
    pure results

-- | Send an expression to a machine to be evaluated and return the result of
-- the evaluation. The state of the machine is not changed.
query :: (Client m) => MachineId -> Value -> m Value
query machineId q = do
    QueryResponse {..} <- runClient $ client machineQueryEndpoint machineId (QueryRequest q)
    pure result

-- | Create a new machine and return its ID.
newMachine :: (Client m) => m MachineId
newMachine = do
    NewResponse {..} <- runClient $ client newMachineEndpoint
    pure machineId


-- | Primitive function defitions for @daemon/send!@, @daemon/query!@, and
-- @daemon/new-machine!@.
daemonClientPrimFns ::(MonadIO m) => PrimFns m
daemonClientPrimFns =
    fromList [ (unsafeToIdent sendName, Nothing, sendPrimFn)
             , (unsafeToIdent queryName, Nothing, queryPrimFn)
             , (unsafeToIdent newMachineName, Nothing, newMachinePrimFn)
             ]
  where
    sendName = "daemon/send!"
    sendPrimFn =
        PrimFns.twoArg sendName $ \case
            (String id, Vec v) -> wrapServantError $ Vec . fromList <$> send (MachineId id) v
            (String _, v) -> throwErrorHere $ TypeError sendName 1 TVec v
            (v, _) -> throwErrorHere $ TypeError sendName 0 TString v

    queryName = "daemon/query!"
    queryPrimFn =
        PrimFns.twoArg queryName $ \case
            (String id, q) -> wrapServantError $ query (MachineId id) q
            (v, _)        -> throwErrorHere $ TypeError queryName 0 TString v

    newMachineName = "daemon/new-machine!"
    newMachinePrimFn [] = do
        MachineId id <- wrapServantError newMachine
        pure $ String id
    newMachinePrimFn args = throwErrorHere $ WrongNumberOfArgs newMachineName 0 (length args)

-- * Internal

runClient :: forall a m. (Client m) => ClientM a -> m a
runClient clientAction = liftEither =<< liftIO runHttp
  where
    runHttp :: IO (Either ServantError a)
    runHttp = do
        baseUrl <- parseBaseUrl "http://localhost:8909"
        manager <- HttpClient.newManager HttpClient.defaultManagerSettings
        let clientEnv = mkClientEnv manager baseUrl
        runClientM clientAction clientEnv

-- | Rethrow Servant error as Radicle language error
--
-- If the Servant error is 'FailureResponse' we wrap the content of the reponse
-- body. Otherwise we use 'displayException'.
wrapServantError :: Monad m => ExceptT ServantError m a -> Lang m a
wrapServantError action = do
    result <- lift $ runExceptT action
    case result of
        Left (FailureResponse response) -> throwErrorHere $ OtherError $ toS $ responseBody response
        Left e -> throwErrorHere $ OtherError $ toS $ displayException e
        Right value                  -> pure value
