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

import           Protolude

import           Radicle (LangError, Value, renderCompactPretty)
import           Radicle.Daemon.Common
import           Radicle.Daemon.Ipfs
import           Radicle.Ipfs


newtype Daemon a = Daemon (ExceptT Error (ReaderT Env IO) a)
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO, MonadReader Env)

runDaemon :: Env -> Daemon a -> IO (Either Error a)
runDaemon env (Daemon x) = runReaderT (runExceptT x) env

liftExceptT :: (e -> Error) -> ExceptT e IO a -> Daemon a
liftExceptT makeError action = Daemon $ mapExceptT (lift . fmap (first makeError)) action

-- * Environment

type FileLock = MVar ()

data LogLevel = Normal | Debug
  deriving (Eq, Ord)

data Env = Env
  { machineConfigFileLock :: FileLock
  , machineConfigFile     :: FilePath
  , machines              :: CachedMachines
  , logLevel              :: LogLevel
  }

-- * Errors

-- | An error relating to a specific machine managed by the daemon.
data MachineError
  = InvalidInput (LangError Value)
  | IpfsError IpfsException
  | AckTimeout
  | DaemonError Text
  | MachineNotCached

-- | An error that the daemon can encounter.
data Error
  = MachineError MachineId MachineError
  | CouldNotCreateMachine IpfsException

displayError :: Error -> (Text, [(Text,Text)])
displayError = \case
  CouldNotCreateMachine (ipfsErr -> (msg, infos)) -> ("Could not create IPFS machine", ("message", msg) : infos)
  MachineError id e -> let mid = ("machine-id", getMachineId id) in
    case e of
      InvalidInput err -> ("Invalid radicle input", [mid, ("radicle-error", renderCompactPretty err)])
      IpfsError (ipfsErr -> (msg, infos)) -> (msg, mid : infos)
      AckTimeout -> ("The machine owner appears to be offline", [mid])
      MachineNotCached -> ("Machine was not found in cache", [mid])
      DaemonError err -> ("Internal error", [mid, ("error", err)])
  where
    ipfsErr = \case
      IpfsException msg -> ("There was an error using the IPFS daemon", [("ipfs-error", msg)])
      IpfsExceptionErrResp msg -> ("The IPFS daemon returned an error", [("ipfs-error", msg)])
      IpfsExceptionErrRespNoMsg -> ("There was an unknown error using IPFS", [])
      IpfsExceptionTimeout -> ("Timeout communicating with IPFS daemon", [])
      IpfsExceptionInvalidResponse url parseError -> ("Cannot parse IPFS daemon response", [("url", url), ("parse-error", parseError)])
      IpfsExceptionNoDaemon -> ("The IPFS daemon is not reachable, run 'rad ipfs daemon'", [])
