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

import qualified Data.Aeson as Aeson

import           Radicle (Value)
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs
import qualified Radicle.Internal.UUID as UUID
import qualified Radicle.Ipfs as Ipfs


newtype MachineId = MachineId Text
    deriving (Show, Eq, Ord, Generic)

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInput | Req ReqInput
    deriving (Show, Eq, Generic)

-- | Message sent to signal a new input has been added to the machine.
data NewInputs = NewInputs
  { nonce   :: Maybe Text
  , results :: [Value]
  }

instance Aeson.FromJSON Message

data NewInput = NewInput
  { nonce   :: Maybe Text
  , results :: [Value]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON NewInput

-- | Message sent to request the writer to add an input to the
-- machine.
data ReqInput = ReqInput
  { nonce       :: Text
  , expressions :: [Value]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON ReqInput

writeIpfs :: MachineId -> [Value] -> IO Ipfs.MachineEntryIndex
writeIpfs = notImplemented

-- | Publish a message on a machine's IPFS pubsub topic.
publish :: MachineId -> Message -> IO ()
publish = notImplemented

-- | Subscribe to messages on a machine's IPFS pubsub topic.
subscribeForever :: MachineId -> (Message -> IO ()) -> IO ()
subscribeForever (MachineId id) messageHandler =
    Ipfs.subscribe topic pubsubHandler
  where
    topic = "radicle:machine:" <> id
    pubsubHandler Ipfs.PubsubMessage{..} =
        case Aeson.decodeStrict messageData of
            Nothing  -> putStrLn ("Cannot parse pubsub message" :: Text)
            Just msg -> messageHandler msg

subscribeOne :: MachineId -> Int -> (Message -> Bool) -> IO (Maybe Message)
subscribeOne = notImplemented

machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom id idx = notImplemented -- Just <$> Ipfs.receiveIpfs id (Just idx)

createMachine :: MonadIO m => m (Either Text MachineId)
createMachine = liftIO $ second MachineId <$> (Ipfs.ipfsMachineCreate =<< UUID.uuid)
