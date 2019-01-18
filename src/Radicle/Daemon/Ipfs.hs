module Radicle.Daemon.Ipfs
  ( MachineId(..)
  , Message(..)
  , NewInput(..)
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
    deriving (Show, Eq, Generic)

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInput | Req ReqInput

data NewInput = NewInput
  { nonce :: Maybe Text }

data ReqInput = ReqInput
  { nonce      :: Maybe Text
  , expression :: [Value]
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
