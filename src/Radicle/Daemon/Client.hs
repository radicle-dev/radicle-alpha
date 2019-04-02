-- | Client for the daemons machine API.
--
-- Also provides primitive functions to bind the client into the Radicle
-- interpreter with @daemonClientPrimFns@.
module Radicle.Daemon.Client
    ( createDaemonClientPrimFns
    , newClient
    , newMachine
    , query
    , send
    , MachineId(..)
    ) where

import           Protolude hiding (TypeError)

import           Control.Monad.Except
import           GHC.Exts (fromList)
import qualified Network.HTTP.Client as HttpClient
import qualified Servant.Client as Servant
import           System.Environment

import           Radicle.Daemon.HttpApi
import           Radicle.Internal.Core
import           Radicle.Internal.Identifier
import qualified Radicle.Internal.PrimFns as PrimFns
import           Radicle.Internal.Type
import qualified Radicle.Ipfs as Ipfs

-- | Constraints for all client functions
type ClientM m = (MonadIO m, MonadError Servant.ServantError m)

data Client = Client
    { clientBaseUrl     :: Servant.BaseUrl
    , clientHttpManager :: HttpClient.Manager
    }

-- | Create a new client for the daemon API. Reads the base URL from the
-- @RAD_DAEMON_API_URL@ environment variable. If the variable is not set we use
-- @http://localhost:8909@
newClient :: IO Client
newClient = do
    baseUrlString <- fromMaybe "http://localhost:8909" <$> lookupEnv "RAD_DAEMON_API_URL"
    clientBaseUrl <- Servant.parseBaseUrl baseUrlString
    clientHttpManager <- Ipfs.newIpfsHttpClientManager
    pure Client {..}

-- | Send an input to a machine to be evaluated and applied. Returns the results
-- of evaluting the inputs.
send :: ClientM m => Client -> MachineId -> Seq Value -> m [Value]
send client machineId inputs = do
    SendResponse {..} <- runClient client $ Servant.client machineSendEndpoint machineId (SendRequest $ toList inputs)
    pure results

-- | Send an expression to a machine to be evaluated and return the result of
-- the evaluation. The state of the machine is not changed.
query :: (ClientM m) => Client -> MachineId -> Value -> m Value
query client machineId q = do
    QueryResponse {..} <- runClient client $ Servant.client machineQueryEndpoint machineId (QueryRequest q)
    pure result

-- | Create a new machine and return its ID.
newMachine :: (ClientM m) => Client -> m MachineId
newMachine client = do
    NewResponse {..} <- runClient client $ Servant.client newMachineEndpoint
    pure machineId


-- | Primitive function defitions for @daemon/send!@, @daemon/query!@, and
-- @daemon/new-machine!@.
createDaemonClientPrimFns ::(MonadIO m) => IO (PrimFns m)
createDaemonClientPrimFns = do
    client <- newClient
    pure $ fromList
        [ (unsafeToIdent sendName, Nothing, sendPrimFn client)
        , (unsafeToIdent queryName, Nothing, queryPrimFn client)
        , (unsafeToIdent newMachineName, Nothing, newMachinePrimFn client)
        ]
  where
    sendName = "daemon/send!"
    sendPrimFn httpManager =
        PrimFns.twoArg sendName $ \case
            (String id, Vec v) -> wrapServantError $ Vec . fromList <$> send httpManager (MachineId id) v
            (String _, v) -> throwErrorHere $ TypeError sendName 1 TVec v
            (v, _) -> throwErrorHere $ TypeError sendName 0 TString v

    queryName = "daemon/query!"
    queryPrimFn httpManager =
        PrimFns.twoArg queryName $ \case
            (String id, q) -> wrapServantError $ query httpManager (MachineId id) q
            (v, _)        -> throwErrorHere $ TypeError queryName 0 TString v

    newMachineName = "daemon/new-machine!"
    newMachinePrimFn httpManager =
        PrimFns.noArg newMachineName $ do
            MachineId id <- wrapServantError $ newMachine httpManager
            pure $ String id

-- * Internal

runClient :: forall a m. (ClientM m) => Client -> Servant.ClientM a -> m a
runClient Client {..} clientAction = do
    let clientEnv = Servant.mkClientEnv clientHttpManager clientBaseUrl
    result <- liftIO $ Servant.runClientM clientAction clientEnv
    liftEither result

-- | Rethrow Servant error as Radicle language error
--
-- If the Servant error is 'FailureResponse' we wrap the content of the reponse
-- body. Otherwise we use 'displayException'.
wrapServantError :: Monad m => ExceptT Servant.ServantError m a -> Lang m a
wrapServantError action = do
    result <- lift $ runExceptT action
    case result of
        Left (Servant.FailureResponse response) -> throwErrorHere $ DaemonError $ toS $ Servant.responseBody response
        Left e -> case e of
          Servant.ConnectionError ce -> throwErrorHere $ DaemonError $ startDaemonHint <> "\n\n" <> ce
          _ -> throwErrorHere $ DaemonError $ toS $ displayException e
        Right value -> pure value
  where
    startDaemonHint = "Could not connect to the Radicle daemon, most likely it is not running. Please see the documentation for how to start it."
