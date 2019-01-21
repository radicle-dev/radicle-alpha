module Radicle.Daemon.Ipfs
  ( MachineId(..)
  , Message(..)
  , NewInputs(..)
  , ReqInputs(..)
  , writeIpfs
  , publish
  , machineInputsFrom
  , createMachine
  , Ipfs.MachineEntryIndex
  , TopicSubscription
  , initSubscription
  , subscribeOne
  , addHandler
  ) where

import           Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Unique as Unique
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import           Radicle (Value)
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs
import qualified Radicle.Internal.UUID as UUID
import qualified Radicle.Ipfs as Ipfs


newtype MachineId = MachineId { getMachineId :: Text }
    deriving (Show, Eq, Ord, Generic)

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInputs | Req ReqInputs
    deriving (Show, Eq, Generic)

instance Aeson.FromJSON Message

-- | Message sent to signal a new input has been added to the machine.
data NewInputs = NewInputs
  { nonce   :: Maybe Text
  , results :: [Value]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON NewInputs

-- | Message sent to request the writer to add an input to the
-- machine.
data ReqInputs = ReqInputs
  { nonce       :: Text
  , expressions :: [Value]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON ReqInputs

writeIpfs :: MachineId -> [Value] -> IO Ipfs.MachineEntryIndex
writeIpfs (MachineId id) vs = Ipfs.sendIpfs id (Seq.fromList vs)

-- | Publish a message on a machine's IPFS pubsub topic.
-- TODO(james): implement
publish :: MachineId -> Message -> IO ()
publish (MachineId id) msg =
  putStrLn $ "[fake] publishing to: " <> id <> " : " <> show msg

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

machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom (MachineId id) = Ipfs.receiveIpfs id

createMachine :: MonadIO m => m (Either Text MachineId)
createMachine = liftIO $ second MachineId <$> (Ipfs.ipfsMachineCreate =<< UUID.uuid)

-- * Topic subscriptions

type MsgHandler = Message -> IO ()

type RegisteredHandler = Unique.Unique

newtype TopicSubscription = TopicSubscription
  { handlers :: MVar (Map RegisteredHandler MsgHandler)
  }

-- | Given a machine ID, creates a subscription for that machine's
-- pubsub topic. This allows adding and removing handlers for messages
-- on that topic.
initSubscription :: MachineId -> IO TopicSubscription
initSubscription id = do
    hdlrs <- newMVar Map.empty
    _ <- forkIO (subscribeForever id (mainHdlr hdlrs))
    pure $ TopicSubscription hdlrs
  where
    mainHdlr hdlrs msg = do
      hs <- readMVar hdlrs
      traverse_ ($ msg) (Map.elems hs)

-- | Add a message handler to a topic subscription.
addHandler :: TopicSubscription -> MsgHandler -> IO RegisteredHandler
addHandler ts h = do
  u <- Unique.newUnique
  modifyMVar (handlers ts) (pure . (,()) . Map.insert u h)
  pure u

-- | Remove a message handler from a topic subscription.
removeHandler :: TopicSubscription -> RegisteredHandler -> IO ()
removeHandler ts u =
  modifyMVar (handlers ts) (pure . (,()) . Map.delete u)

-- | Wait for a message which matches a predicate. Returns @Just msg@
-- where @msg@ is the first message passing the predicate if such a
-- message arrives before the specified amount of
-- milliseconds. Otherwise, returns @Nothing@.
subscribeOne :: TopicSubscription -> Int -> (Message -> Bool) -> IO (Maybe Message)
subscribeOne sub timeout pr = do
  var <- newEmptyMVar
  let onMsg = \case
        msg | pr msg -> putMVar var (Just msg)
        _ -> pure ()
  h <- addHandler sub onMsg
  _ <- forkIO (threadDelay (timeout * 1000) >> putMVar var Nothing)
  res <- readMVar var
  removeHandler sub h
  pure res
