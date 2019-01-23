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

import           Control.Exception.Safe hiding (bracket)
import           Control.Monad.Fail
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Unique as Unique

import           Radicle
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs
import qualified Radicle.Internal.UUID as UUID
import qualified Radicle.Ipfs as Ipfs

-- * Types

newtype JsonValue = JsonValue { jsonValue :: Value }
  deriving (Show)

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

instance Aeson.ToJSONKey MachineId where
  toJSONKey = Aeson.toJSONKeyText getMachineId
instance Aeson.FromJSONKey MachineId where
  fromJSONKey = Aeson.FromJSONKeyText MachineId
instance Aeson.ToJSON MachineId where
  toJSON (MachineId id) = Aeson.String id
instance Aeson.FromJSON MachineId where
  parseJSON js = MachineId <$> Aeson.parseJSON js

-- | Messages sent on a machine's IPFS pubsub topic.
data Message = New NewInputs | Req ReqInputs

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \o -> do
    typ :: Text <- o .: "type"
    case typ of
      "new_inputs" -> do
        nonce <- o .: "nonce"
        results <- o .: "results"
        pure $ New NewInputs{..}
      "request_inputs" -> do
        nonce <- o .: "nonce"
        expressions <- o .: "expressions"
        pure $ Req ReqInputs{..}
      _ -> fail "Messages on a machine's pubsub topic must be of type 'new_inputs' or 'request_inputs'."

instance Aeson.ToJSON Message where
  toJSON = \case
    New NewInputs{..} -> Aeson.object
      [ "type" .= ("new_inputs" :: Text)
      , "nonce" .= nonce
      , "results" .= results
      ]
    Req ReqInputs{..} -> Aeson.object
      [ "type" .= ("request_inputs" :: Text)
      , "nonce" .= nonce
      , "expressions" .= expressions
      ]

-- | Message sent to signal a new input has been added to the machine.
data NewInputs = NewInputs
  { nonce   :: Maybe Text
  , results :: [JsonValue]
  }

-- | Message sent to request the writer add inputs to the machine.
data ReqInputs = ReqInputs
  { nonce       :: Text
  , expressions :: [JsonValue]
  }

-- * Ipfs helpers

safeIpfs :: IO a -> ExceptT Text IO a
safeIpfs io = ExceptT $ do
  res <- liftIO $ tryAny io
  pure $ first (toS . displayException) res

-- | Write some inputs to an IPFS machine.
-- TODO: we might want to make this safer by taking the expected head
-- MachineEntryIndex as an argument.
writeIpfs :: MachineId -> [Value] -> ExceptT Text IO Ipfs.MachineEntryIndex
writeIpfs (MachineId id) vs = safeIpfs $ Ipfs.sendIpfs id (Seq.fromList vs)

newtype Topic = Topic { getTopic :: Text }

-- | The IPFS pubsub topic associated to a machine.
machineTopic :: MachineId -> Topic
machineTopic (MachineId id) = Topic ("radicle:machine:" <> id)

-- | Publish a 'Message' on a machine's IPFS pubsub topic.
publish :: MachineId -> Message -> ExceptT Text IO ()
publish id msg = safeIpfs $ do
  liftIO $ putStrLn $ "pubsub pub " <> getMachineId id <> ": " <> toS (Aeson.encode msg)
  Ipfs.publish (getTopic (machineTopic id)) (Aeson.encode msg)

-- | Subscribe to messages on a machine's IPFS pubsub topic.
subscribeForever :: MachineId -> (Message -> IO ()) -> IO ()
subscribeForever id messageHandler = Ipfs.subscribe topic pubsubHandler
  where
    Topic topic = machineTopic id
    pubsubHandler Ipfs.PubsubMessage{..} =
        case Aeson.decodeStrict messageData of
            Nothing  -> putStrLn ("Cannot parse pubsub message" :: Text)
            Just msg -> messageHandler msg

-- | Get inputs of an IPFS machine from a certain index.
machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> ExceptT Text IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom (MachineId id) = safeIpfs . Ipfs.receiveIpfs id

-- | Create an IPFS machine and return its ID.
createMachine :: ExceptT Text IO MachineId
createMachine = ExceptT $ liftIO $ second MachineId <$> (Ipfs.ipfsMachineCreate =<< UUID.uuid)

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
initSubscription id = safeIpfs $ do
    hdlrs <- newMVar Map.empty
    _ <- async $ subscribeForever id (mainHdlr hdlrs)
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

-- | Block while waiting for a message which matches a
-- predicate. Returns @Just msg@ where @msg@ is the first message
-- passing the predicate if such a message arrives before the
-- specified amount of milliseconds. Otherwise, returns @Nothing@.
subscribeOne :: TopicSubscription -> Int -> (Message -> Bool) -> IO (Maybe Message)
subscribeOne sub timeout pr = do
  var <- newEmptyMVar
  let onMsg = \case
        msg | pr msg -> putMVar var (Just msg)
        _ -> pure ()
  bracket
    (addHandler sub onMsg)
    (removeHandler sub)
    (const $ forkIO (threadDelay (timeout * 1000) >> putMVar var Nothing) *> readMVar var)
