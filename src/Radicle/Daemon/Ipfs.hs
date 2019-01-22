module Radicle.Daemon.Ipfs
  ( MachineId(..)
  , JsonValue(..)
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

import           Control.Exception.Safe
import           Control.Monad.Fail
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Unique as Unique

import           Radicle
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs
import qualified Radicle.Internal.UUID as UUID
import qualified Radicle.Ipfs as Ipfs

-- * Types

newtype JsonValue = JsonValue { jsonValue :: Value }

instance Aeson.FromJSON JsonValue where
  parseJSON = Aeson.withText "JsonValue" $ \t -> do
      v <- case parse "[daemon]" t of
        Left err -> fail $ "failed to parse Radicle expression: " <> show err
        Right v  -> pure v
      pure (JsonValue v)

instance Aeson.ToJSON JsonValue where
  toJSON (JsonValue v) = Aeson.String $ renderCompactPretty v

newtype MachineId = MachineId { getMachineId :: Text }
    deriving (Show, Eq, Ord, Generic)

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInputs | Req ReqInputs
    deriving (Generic)

-- TODO(james): write these by hand to conform to the spec.
instance Aeson.FromJSON Message
instance Aeson.ToJSON Message

-- | Message sent to signal a new input has been added to the machine.
data NewInputs = NewInputs
  { nonce   :: Maybe Text
  , results :: [JsonValue]
  } deriving (Generic)

instance Aeson.FromJSON NewInputs
instance Aeson.ToJSON NewInputs

-- | Message sent to request the writer to add an input to the
-- machine.
data ReqInputs = ReqInputs
  { nonce       :: Text
  , expressions :: [JsonValue]
  } deriving (Generic)

instance Aeson.FromJSON ReqInputs
instance Aeson.ToJSON ReqInputs

-- * Ipfs helpers

safeIpfs :: IO a -> ExceptT Text IO a
safeIpfs io = ExceptT $ do
  res <- liftIO $ tryAny $ io
  pure $ first (toS . displayException) res

writeIpfs :: MachineId -> [Value] -> ExceptT Text IO Ipfs.MachineEntryIndex
writeIpfs (MachineId id) vs = safeIpfs $ Ipfs.sendIpfs id (Seq.fromList vs)

-- | Publish a 'Message' on a machine's IPFS pubsub topic.
publish :: MachineId -> Message -> ExceptT Text IO ()
publish (MachineId id) msg = safeIpfs $ Ipfs.publish id (Aeson.encode msg)

-- | Subscribe to messages on a machine's IPFS pubsub topic.
subscribeForever :: MachineId -> (Message -> IO ()) -> ExceptT Text IO ()
subscribeForever (MachineId id) messageHandler = safeIpfs $
    Ipfs.subscribe topic pubsubHandler
  where
    topic = "radicle:machine:" <> id
    pubsubHandler Ipfs.PubsubMessage{..} =
        case Aeson.decodeStrict messageData of
            Nothing  -> putStrLn ("Cannot parse pubsub message" :: Text)
            Just msg -> messageHandler msg

machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> ExceptT Text IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom (MachineId id) = safeIpfs . Ipfs.receiveIpfs id

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
initSubscription :: MachineId -> ExceptT Text IO TopicSubscription
initSubscription id = do
    hdlrs <- liftIO $ newMVar Map.empty
    -- TODO(james): propagate errors
    _ <- liftIO $ forkIO (liftIO (runExceptT (subscribeForever id (mainHdlr hdlrs)) >> pure ()))
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
