-- | This module defines a 'Machine' as the in-memory representation of
-- a Radicle State Machine and a 'MonadMachineStore' class that
-- provides access to a set of machines in memory.
module Radicle.Daemon.MachineStore
  ( Machine(..)
  , ReaderOrWriter(..)

  , Polling(..)
  , highFreq

  , CachedMachine(..)
  , CachedMachines(..)
  , MonadMachineStore(..)
  , modifyMachine
  , modifyMachine'
  , insertMachine
  , insertUninitializedReader
  , emptyMachine
  , traverseMachines
  , lookupMachine
  ) where

import           Protolude

import           Control.Exception.Safe
import           Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Time.Clock.System as Time

import           Radicle
import           Radicle.Daemon.Error
import           Radicle.Daemon.Ipfs
import qualified Radicle.Internal.ConcurrentMap as CMap

-- | Indicates if the daemon is the writer or a reader for this machine.
data ReaderOrWriter = Reader | Writer
  deriving (Generic)

instance A.ToJSON ReaderOrWriter
instance A.FromJSON ReaderOrWriter


data Polling
  -- | Indicates that the daemon is performing high-frequency polling for this
  -- machine. Includes the number of milliseconds for how long this will
  -- continue being the case.
  = HighFreq Int64
  -- | Indicates that the daemon is only performing low-frequency polling for
  -- this machine.
  | LowFreq

-- | The amount of time a machine does high-frequency polling for
-- before it returns to low-frequency polling: 10 minutes.
highFreq :: Polling
highFreq = HighFreq (10 * 60 * 1000)


-- A Cached machine with an initialised state and pubsub subscription.
data Machine = Machine
    { machineId           :: MachineId
    , machineState        :: Bindings (PrimFns Identity)
    , machineLastIndex    :: MachineEntryIndex
    , machineMode         :: ReaderOrWriter
    , machineSubscription :: TopicSubscription
    , machineLastUpdated  :: Time.SystemTime
    , machinePolling      :: Polling
    } deriving (Generic)

data CachedMachine
  -- | A reader which could not be initialised (probably due to IPNS name not
  -- resolving). Includes the time at which the last failed init was attempted.
  = UninitialisedReader Time.SystemTime
  -- | A cached machine with an active pubsub subscription.
  | Cached Machine

newtype CachedMachines = CachedMachines { getMachines :: CMap.CMap MachineId CachedMachine }

-- * Machine access and modification

class (MonadUnliftIO m, MonadThrow m) => MonadMachineStore m where
    askMachines :: m CachedMachines


-- | Modify a machine that is already in the store. Errors if the
-- machine isn't in the store or is 'UninitializedReader'. Concurrent
-- access to the same machine is serialized.
modifyMachine :: forall a m. (MonadMachineStore m) => MachineId -> (Machine -> m (Machine, a)) -> m a
modifyMachine id f = do
    machines <- askMachines
    res <- CMap.modifyExistingValue id (getMachines machines) modCached
    case res of
      Nothing -> throw (MachineError id MachineNotCached)
      Just a  -> pure a
  where
    modCached :: CachedMachine -> m (CachedMachine, a)
    modCached = \case
        UninitialisedReader _ -> throw $ MachineError id MachineNotCached
        Cached m -> do
            (m', a) <- f m
            pure (Cached m', a)

modifyMachine' :: forall a m. (MonadMachineStore m) => MachineId -> (Maybe CachedMachine -> m (Maybe CachedMachine, a)) -> m a
modifyMachine' id f = do
    machines <- askMachines
    CMap.modifyValue id (getMachines machines) f

emptyMachine :: (MonadMachineStore m) => MachineId -> ReaderOrWriter -> TopicSubscription -> m Machine
emptyMachine id mode sub = liftIO $ do
  t <- Time.getSystemTime
  pure Machine{ machineId = id
              , machineState = pureEnv
              , machineLastIndex = emptyMachineEntryIndex
              , machineMode = mode
              , machineSubscription = sub
              , machineLastUpdated = t
              , machinePolling = highFreq
              }

insertMachine :: (MonadMachineStore m) => Machine -> m ()
insertMachine m = insertCachedMachine (machineId m) (Cached m)

-- | Register a machine as a new 'UninitailizedReader' in the store.
insertUninitializedReader :: (MonadMachineStore m) => MachineId -> m ()
insertUninitializedReader id = do
    t <- liftIO Time.getSystemTime
    insertCachedMachine id (UninitialisedReader t)

-- | Run @f@ sequentially for every machine in the store.
traverseMachines :: (MonadMachineStore m) => (MachineId -> CachedMachine -> m b) -> m ()
traverseMachines f = do
    msVar <- askMachines
    ms <- liftIO $ CMap.nonAtomicRead (getMachines msVar)
    void $ Map.traverseWithKey f ms

lookupMachine :: (MonadMachineStore m) => MachineId -> m (Maybe CachedMachine)
lookupMachine id = do
  msCMap <- askMachines
  liftIO $ CMap.lookup id (getMachines msCMap)

insertCachedMachine :: (MonadMachineStore m) => MachineId -> CachedMachine -> m ()
insertCachedMachine id m = do
    msCMap <- askMachines
    liftIO $ CMap.insert id m (getMachines msCMap)
