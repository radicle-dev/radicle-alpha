module Radicle.Daemon.Ipfs
  ( MachineId(..)
  , Message(..)
  , NewInputs(..)
  , ReqInput(..)
  , writeIpfs
  , publish
  , subscribeForever
  , subscribeOne
  , machineInputsFrom
  , createMachine

  , Ipfs.MachineEntryIndex
  ) where

import           Protolude

import           Radicle (Value)
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs
import qualified Radicle.Internal.UUID as UUID


newtype MachineId = MachineId Text
    deriving (Show, Eq, Ord, Generic)

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInputs | Req ReqInput

-- | Message sent to signal a new input has been added to the machine.
data NewInputs = NewInputs
  { nonce :: Maybe Text
  , results :: [Value]
  }

-- | Message sent to request the writer to add an input to the
-- machine.
data ReqInput = ReqInput
  { nonce       :: Text
  , expressions :: [Value]
  }


writeIpfs :: MachineId -> [Value] -> IO Ipfs.MachineEntryIndex
writeIpfs = notImplemented

-- | Publish a message on a machine's IPFS pubsub topic.
publish :: MachineId -> Message -> IO ()
publish = notImplemented

-- | Subscribe to messages on a machine's IPFS pubsub topic.
-- Takes an optional timeout.
subscribeForever :: MachineId -> (Message -> IO ()) -> IO ()
subscribeForever = notImplemented

subscribeOne :: MachineId -> Int -> (Message -> Bool) -> IO (Maybe Message)
subscribeOne = notImplemented

machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom id idx = notImplemented -- Just <$> Ipfs.receiveIpfs id (Just idx)

createMachine :: MonadIO m => m (Either Text MachineId)
createMachine = liftIO $ second MachineId <$> (Ipfs.ipfsMachineCreate =<< UUID.uuid)
