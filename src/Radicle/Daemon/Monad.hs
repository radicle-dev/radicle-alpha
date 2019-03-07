-- | Defines the 'Daemon' monad in which all actions are exectued.
-- 'Daemon' is an instance of @'MonadReader' 'Env'@, @'MonadError'
-- 'Error'@, and 'MonadIO'.
module Radicle.Daemon.Monad
    ( Daemon
    , runDaemon
    , liftExceptT

    , Env(..)
    , FileLock
    , LogLevel(..)

    , MachineError(..)
    , Error(..)
    , displayError
    ) where

import           Protolude hiding (try)

import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.IO.Unlift

import           Radicle.Daemon.Error
import           Radicle.Daemon.Logging
import           Radicle.Daemon.MachineStore


newtype Daemon a = Daemon (ExceptT Error (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO, MonadReader Env)

instance MonadUnliftIO Daemon where
    withRunInIO inner = do
        env <- ask
        result <- liftIO $ try $ inner $ \d -> do
            result <- runDaemon env d
            case result of
                Left err -> throw err
                Right a  -> pure a
        liftEither result

runDaemon :: Env -> Daemon a -> IO (Either Error a)
runDaemon env (Daemon x) = runReaderT (runExceptT x) env

liftExceptT :: (e -> Error) -> ExceptT e IO a -> Daemon a
liftExceptT makeError action = Daemon $ mapExceptT (lift . fmap (first makeError)) action

instance MonadLog Daemon where
    askLogLevel = asks logLevel

instance MonadMachineStore Daemon where
    askMachines = asks machines

-- * Environment

type FileLock = MVar ()

data Env = Env
  { machineConfigFileLock :: FileLock
  , machineConfigFile     :: FilePath
  , machines              :: CachedMachines
  , logLevel              :: LogLevel
  }
