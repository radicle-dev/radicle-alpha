-- | Defines the 'Daemon' monad in which all actions are exectued.
-- 'Daemon' is an instance of @'MonadReader' 'Env'@, @'MonadError'
-- 'Error'@, and 'MonadIO'.
module Radicle.Daemon.Monad
    ( Daemon
    , runDaemon
    , liftExceptT

    , Env(..)
    , FollowFileLock
    , LogLevel(..)

    , MachineError(..)
    , Error(..)
    , displayError
    ) where

import           Protolude

import           Radicle (LangError, Value, renderCompactPretty)
import           Radicle.Daemon.Common
import           Radicle.Daemon.Ipfs


newtype Daemon a = Daemon (ExceptT Error (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO, MonadReader Env)

runDaemon :: Env -> Daemon a -> IO (Either Error a)
runDaemon env (Daemon x) = runReaderT (runExceptT x) env

liftExceptT :: (e -> Error) -> ExceptT e IO a -> Daemon a
liftExceptT makeError action = Daemon $ mapExceptT (lift . fmap (first makeError)) action

-- * Environment

type FollowFileLock = MVar ()

data LogLevel = Normal | Debug
  deriving (Eq, Ord)

data Env = Env
  { followFileLock :: FollowFileLock
  , followFile     :: FilePath
  , machines       :: CachedMachines
  , logLevel       :: LogLevel
  }

-- * Errors

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
        Timeout -> ("Timeout communicating with IPFS daemon", [mid])
      AckTimeout -> ("The writer appears to be offline", [mid])
      MachineAlreadyCached -> ("Tried to add already cached machine", [mid])
      MachineNotCached -> ("Machine was not found in cache", [mid])
