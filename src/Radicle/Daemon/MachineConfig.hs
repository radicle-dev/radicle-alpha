-- | This module provides disk persistence for 'MachineConfig'.
-- 'MachineConfig' tells us which machine the daemon owns and which
-- machines it follows.
module Radicle.Daemon.MachineConfig
    ( MachineConfig(..)
    , MachinesConfig
    , readMachineConfigIO
    , writeMachineConfig
    ) where

import           Protolude

import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import           System.Directory (doesFileExist)

import           Radicle.Daemon.Common
import           Radicle.Daemon.Ipfs
import           Radicle.Daemon.Monad
import qualified Radicle.Internal.ConcurrentMap as CMap

data MachineConfig
  = ConfigReader
  | ConfigWriter MachineEntryIndex
  deriving (Generic)

instance A.ToJSON MachineConfig
instance A.FromJSON MachineConfig

-- | Holds the machines the daemon owns and follows.
type MachinesConfig = Map MachineId MachineConfig

readMachineConfigIO :: FileLock -> FilePath -> IO MachinesConfig
readMachineConfigIO lock ff = withFileLock lock $ do
  exists <- doesFileExist ff
  t <- if exists
    then readFile ff
    else let noFollows = toS (A.encode (Map.empty :: MachinesConfig))
         in writeFile ff noFollows $> noFollows
  case A.decode (toS t) of
    Nothing -> panic $ "Invalid daemon-follow file: could not decode " <> toS ff
    Just fs -> pure fs

writeMachineConfig :: Daemon ()
writeMachineConfig = do
    lock <- asks machineConfigFileLock
    CachedMachines cMap <- asks machines
    ff <- asks machineConfigFile
    liftIO $ withFileLock lock $ do
      ms <- CMap.nonAtomicRead cMap
      let fs = mode <$> ms
      writeFile ff (toS (A.encode fs))
  where
    mode (Cached Machine{machineMode = Writer, ..}) = ConfigWriter machineLastIndex
    mode _ = ConfigReader

withFileLock :: FileLock -> IO a -> IO a
withFileLock lock = withMVar lock . const
