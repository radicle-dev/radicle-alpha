module Radicle.DaemonClient
    ( daemonClientPrimFns
    ) where

import           Protolude hiding (TypeError)

import           GHC.Exts (fromList)

import           Control.Exception.Safe
import qualified Network.HTTP.Client as HttpClient
import           Servant.API
import           Servant.Client

import           Radicle
import           Radicle.Daemon.HttpApi
import qualified Radicle.Internal.PrimFns as PrimFns

daemonClientPrimFns ::(MonadIO m) => PrimFns m
daemonClientPrimFns =
    fromList [ (unsafeToIdent sendName, Nothing, sendPrimFn)
             , (unsafeToIdent queryName, Nothing, queryPrimFn)]
  where
    sendName = "daemon/send!"
    sendPrimFn =
        PrimFns.twoArg sendName $ \case
            (String id, Vec v) -> lift $ Vec . fromList <$> send id v
            (String _, v) -> throwErrorHere $ TypeError sendName 1 TVec v
            (v, _) -> throwErrorHere $ TypeError sendName 0 TString v


    queryName = "daemon/query!"
    queryPrimFn =
        PrimFns.twoArg queryName $ \case
            (String id, q) -> lift $ query id q
            (v, _)        -> throwErrorHere $ TypeError queryName 0 TString v

clientQuery :: MachineId -> Expression -> ClientM Expression
clientSend :: MachineId -> Expressions -> ClientM SendResult
_clientNew :: ClientM NewResult
clientQuery :<|> clientSend :<|> _clientNew = client daemonApi

send :: MonadIO m => Text -> Seq Value -> m [Value]
send machineId inputs = liftIO $ do
    result <- runClient $ clientSend (MachineId machineId) (Expressions (toList $ map JsonValue inputs))
    case result of
        Left err                   -> throwM err
        Right (SendResult _values) -> pure []

query :: (MonadIO m) => Text -> Value -> m Value
query machineId q = liftIO $ do
    result <- runClient $ clientQuery (MachineId machineId) (Expression (JsonValue q))
    case result of
        Left err                             -> throwM err
        Right (Expression (JsonValue value)) -> pure value

runClient :: ClientM a -> IO (Either ServantError a)
runClient clientAction = do
    baseUrl <- parseBaseUrl "http://localhost:8909"
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    runClientM clientAction clientEnv
