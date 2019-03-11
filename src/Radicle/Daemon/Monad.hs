-- | Defines the 'Daemon' monad in which all actions are exectued.
-- 'Daemon' is an instance of @'MonadReader' 'Env'@, @'MonadError'
-- 'Error'@, and 'MonadIO'.
module Radicle.Daemon.Monad
    ( Daemon
    , runDaemon
    , wrapException

    , Env(..)
    , FileLock
    , LogLevel(..)

    , MachineError(..)
    , Error(..)
    , displayError
    ) where

import           Protolude hiding (catch, try)

import           Control.Exception.Safe
import           Control.Monad.IO.Unlift

import           Radicle.Daemon.Error
import           Radicle.Daemon.Ipfs (MonadMachineIpfs)
import           Radicle.Daemon.Logging
import           Radicle.Daemon.MachineStore


newtype Daemon a = Daemon (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadReader Env, MonadUnliftIO)

runDaemon :: Env -> Daemon a -> IO (Either Error a)
runDaemon env (Daemon x) = try $ runReaderT x env

wrapException :: (MonadCatch m, Exception e, Exception e') => (e -> e') -> m a -> m a
wrapException f action = action `catch` (throw . f)

instance MonadLog Daemon where
    askLogLevel = asks logLevel

instance MonadMachineStore Daemon where
    askMachines = asks machines

instance MonadMachineIpfs Daemon

-- * Environment

type FileLock = MVar ()

data Env = Env
  { machineConfigFileLock :: FileLock
  , machineConfigFile     :: FilePath
  , machines              :: CachedMachines
  , logLevel              :: LogLevel
  }
