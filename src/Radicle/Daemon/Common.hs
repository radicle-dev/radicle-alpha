module Radicle.Daemon.Common
  ( ReaderOrWriter(..)
  , Polling(..)
  , Machine(..)
  , CachedMachine(..)
  , CachedMachines(..)
  , logInfo
  , logErr
  , advanceChain
  ) where

import           Protolude hiding (log)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Time.Clock.System as Time

import           Radicle
import           Radicle.Daemon.Ipfs
import qualified Radicle.Internal.ConcurrentMap as CMap

-- | Indicates if the daemon is the writer or a reader for this machine.
data ReaderOrWriter = Reader | Writer
  deriving (Generic)

data Polling
  -- | Indicates that the daemon is performing high-frequency polling for this
  -- machine. Includes the number of milliseconds for how long this will
  -- continue being the case.
  = HighFreq Int64
  -- | Indicates that the daemon is only performing low-frequency polling for
  -- this machine.
  | LowFreq

instance A.ToJSON ReaderOrWriter
instance A.FromJSON ReaderOrWriter

-- A Cached machine with an initialised state and pubsub subscription.
data Machine = Machine
    { machineId           :: MachineId
    , machineState        :: Bindings (PrimFns Identity)
    , machineLastIndex    :: Maybe MachineEntryIndex
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

-- | Log a message to @stdout@.
--
-- The second argument is a list of key-value pairs that will be joined with "="
-- and appended to the message.
--
-- @
--      log "INFO" "the message" [("foo", "5)]
--      -- prints "INFO   the message foo=5"
-- @
log :: MonadIO m => Text -> Text -> [(Text, Text)] -> m ()
log typ msg dat = do
    putStrLn $ typ <> "  " <> msg <> datString
  where
    datString = T.intercalate "" $ map (\(key, val) -> " " <> key <> "=" <> val) dat

logInfo, logErr :: MonadIO m => Text -> [(Text, Text)] -> m ()
logInfo = log "INFO "
logErr  = log "ERROR"

advanceChain :: Machine -> [Value] -> Either (LangError Value) ([Value], Bindings (PrimFns Identity))
advanceChain chain vals =
  let (r_, newSt) = runIdentity $ runLang (machineState chain) $ traverse eval vals
  in case r_ of
    Left e  -> Left e
    Right r -> pure (r, newSt)
