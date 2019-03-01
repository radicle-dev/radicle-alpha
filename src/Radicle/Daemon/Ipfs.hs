module Radicle.Daemon.Ipfs
  ( MachineId(..)
  , Message(..)
  , InputsApplied(..)
  , SubmitInputs(..)
  , writeIpfs
  , Radicle.Daemon.Ipfs.publish
  , ipnsPublish
  , machineInputsFrom
  , createMachine
  , Ipfs.MachineEntryIndex
  , emptyMachineEntryIndex
  , TopicSubscription
  , initSubscription
  , subscribeOne
  , addHandler
  , valueToJson
  , jsonToValue
  ) where

import           Protolude hiding (bracket, catches)

import           Control.Exception.Safe
import           Control.Monad.Fail
import           Data.Aeson (decodeStrict, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Unique as Unique
import           System.Timeout

import           Radicle.Internal.Core
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import qualified Radicle.Internal.UUID as UUID
import qualified Radicle.Ipfs as Ipfs

jsonToValue :: Aeson.Value -> Aeson.Parser Value
jsonToValue = Aeson.withText "Value" $ \t -> do
    case parse "[daemon]" t of
      Left err -> fail $ "failed to parse Radicle expression: " <> show err
      Right v  -> pure v

valueToJson :: Value -> Aeson.Value
valueToJson = Aeson.String . renderCompactPretty

-- * Types

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
data Message = New InputsApplied | Submit SubmitInputs

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \o -> do
    typ :: Text <- o .: "type"
    case typ of
      "new_inputs" -> do
        nonce <- o .: "nonce"
        results <- traverse jsonToValue =<< o .: "results"
        pure $ New InputsApplied{..}
      "request_inputs" -> do
        nonce <- o .: "nonce"
        expressions <- traverse jsonToValue =<< o .: "expressions"
        pure $ Submit SubmitInputs{..}
      _ -> fail "Messages on a machine's pubsub topic must be of type 'new_inputs' or 'request_inputs'."

instance Aeson.ToJSON Message where
  toJSON = \case
    New InputsApplied{..} -> Aeson.object
      [ "type" .= ("new_inputs" :: Text)
      , "nonce" .= nonce
      , "results" .= (valueToJson <$> results)
      ]
    Submit SubmitInputs{..} -> Aeson.object
      [ "type" .= ("request_inputs" :: Text)
      , "nonce" .= nonce
      , "expressions" .= (valueToJson <$> expressions)
      ]

-- | Message sent to signal a new input has been added to the machine.
data InputsApplied = InputsApplied
  { nonce   :: Maybe Text
  , results :: [Value]
  }

-- | Message sent to request the writer add inputs to the machine.
data SubmitInputs = SubmitInputs
  { nonce       :: Text
  , expressions :: [Value]
  }

-- * Ipfs helpers

-- | Write and pin some inputs to an IPFS machine.
-- TODO: we might want to make this safer by taking the expected head
-- MachineEntryIndex as an argument.
writeIpfs :: MachineId -> [Value] -> IO Ipfs.MachineEntryIndex
writeIpfs (MachineId id) vs = Ipfs.sendIpfs id (Seq.fromList vs)

newtype Topic = Topic { getTopic :: Text }

-- | The IPFS pubsub topic associated to a machine.
machineTopic :: MachineId -> Topic
machineTopic (MachineId id) = Topic ("radicle:machine:" <> id)

-- | Publish a 'Message' on a machine's IPFS pubsub topic.
publish :: MachineId -> Message -> IO ()
publish id msg = Ipfs.publish (getTopic (machineTopic id)) (Aeson.encode msg)

-- | Subscribe to messages on a machine's IPFS pubsub topic.
subscribeForever :: MachineId -> (Either Text Message -> IO ()) -> IO ()
subscribeForever id messageHandler = Ipfs.subscribe topic pubsubHandler
  where
    Topic topic = machineTopic id
    pubsubHandler Ipfs.PubsubMessage{..} = messageHandler $ case decodeStrict messageData of
      Just msg -> Right msg
      Nothing  -> Left (toS messageData)

-- | Get and pin inputs of an IPFS machine from a certain index.
machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom = Ipfs.receiveIpfs . getMachineId

-- | Create an IPFS machine and return its ID.
--
-- Generates a new IPNS key and sets the IPNS record to the empty
-- machine DAG node.
createMachine :: IO MachineId
createMachine = do
    uuid <- UUID.uuid
    Ipfs.KeyGenResponse ipnsId <- Ipfs.keyGen uuid
    Ipfs.namePublish ipnsId $ Ipfs.AddressIpfs Ipfs.emptyMachineCid
    pure $ MachineId ipnsId

-- | Publish to IPNS a machine entry index for a machine. Should only be issued
-- by the writer.
ipnsPublish :: MachineId -> Ipfs.MachineEntryIndex -> IO ()
ipnsPublish id (Ipfs.MachineEntryIndex cid) = Ipfs.namePublish (getMachineId id) $ Ipfs.AddressIpfs cid

-- * Topic subscriptions

type MsgHandler = Message -> IO ()

type RegisteredHandler = Unique.Unique

newtype TopicSubscription = TopicSubscription
  { handlers :: MVar (Map RegisteredHandler MsgHandler)
  }

-- | Given a machine ID, creates a subscription for that machine's
-- pubsub topic. This allows adding and removing handlers for messages
-- on that topic.
--
-- @logParseError@ is called with the raw message content if a message
-- cannot be parsed.
initSubscription :: MachineId -> (Text -> IO ()) -> IO TopicSubscription
initSubscription id logParseError = do
    hdlrs <- newMVar Map.empty
    _ <- async $ subscribeForever id (mainHdlr hdlrs)
    pure $ TopicSubscription hdlrs
  where
    mainHdlr hdlrs = \case
        Left rawMessage -> logParseError rawMessage
        Right msg -> do
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

-- | Block while waiting for a matching message.
-- If no matching message arrives within the given timeout (in
-- milliseconds) then @Nothing@ is retuend.
subscribeOne :: TopicSubscription -> Int64 -> (Message -> Maybe a) -> IO (Maybe a)
subscribeOne subscription t matchMessage = do
    timeout (fromIntegral t * 1000) $ do
        msgVar <- newEmptyMVar
        let onMsg msg = case matchMessage msg of
                            Just a  -> putMVar msgVar a
                            Nothing -> pure ()
        bracket
            (addHandler subscription onMsg)
            (removeHandler subscription)
            (\_ -> readMVar msgVar)

emptyMachineEntryIndex :: Ipfs.MachineEntryIndex
emptyMachineEntryIndex = Ipfs.MachineEntryIndex Ipfs.emptyMachineCid
