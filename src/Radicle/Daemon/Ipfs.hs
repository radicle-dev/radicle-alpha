module Radicle.Daemon.Ipfs
  ( MachineId(..)
  , MonadMachineIpfs
  , Message(..)
  , InputsApplied(..)
  , SubmitInputs(..)
  , writeIpfs
  , publish
  , ipnsPublish
  , machineInputsFrom
  , createMachine
  , emptyMachineEntryIndex
  , MachineEntryIndex
  , TopicSubscription
  , initSubscription
  , subscribeOne
  , addHandler
  , valueToJson
  , jsonToValue
  ) where

import           Protolude hiding (async, bracket, catches)

import           Control.Exception.Safe
import           Control.Monad.Fail
import           Control.Monad.IO.Unlift
import           Data.Aeson (decodeStrict, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.IPLD.CID
import qualified Data.Map.Strict as Map
import qualified Data.Unique as Unique
import           System.Timeout
import           UnliftIO.Async

import           Radicle.Internal.Core
import           Radicle.Internal.Json
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import qualified Radicle.Internal.UUID as UUID
import qualified Radicle.Ipfs as Ipfs

-- * Types


class (MonadIO m, MonadThrow m, MonadUnliftIO m, Ipfs.MonadIpfs m) => MonadMachineIpfs m

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


-- | A node in the linked list of inputs of a machine.
--
-- Stored as an IPLD document with the IPFS DAG API. The document has
-- the following shape
-- @
--      {
--        "expressions": [
--          "(def foo :hey)",
--          "(get-value)"
--        ],
--        "previous": { "/": "QmA..." }
--      }
-- @
data MachineEntry = MachineEntry
    { entryExpressions :: [Value]
    , entryPrevious    :: CID
    } deriving (Eq, Show, Read, Generic)

instance Aeson.FromJSON MachineEntry where
    parseJSON = Aeson.withObject "MachineEntry" $ \o -> do
        expressionCodes <- o .: "expressions"
        let src = "[ipfs]"
        entryExpressions :: [Value] <-
            case traverse (parse src) expressionCodes of
                Left err  -> fail $ "failed to parse Radicle expression: " <> show err
                Right v -> pure v
        entryPrevious <- Ipfs.parseIpldLink =<< o .: "previous"
        pure MachineEntry {..}

instance Aeson.ToJSON MachineEntry where
    toJSON MachineEntry{..} =
        let code = map renderCompactPretty entryExpressions
        in Aeson.object
            [ "expressions" .= code
            , "previous" .= Ipfs.ipldLink entryPrevious
            ]

-- | Identifies a 'MachineEntry' stored on IPFS. Wraps 'CID'.
newtype MachineEntryIndex = MachineEntryIndex CID
    deriving (Show, Eq)

instance Aeson.ToJSON MachineEntryIndex where
  toJSON (MachineEntryIndex cid) = Ipfs.ipldLink cid

instance Aeson.FromJSON MachineEntryIndex where
  parseJSON = fmap MachineEntryIndex . Ipfs.parseIpldLink


-- * Ipfs helpers

-- | Write and pin some inputs to an IPFS machine.
-- TODO: we might want to make this safer by taking the expected head
-- MachineEntryIndex as an argument.
writeIpfs :: (MonadMachineIpfs m) => MachineId -> [Value] -> m MachineEntryIndex
writeIpfs (MachineId ipnsId) values = do
    Ipfs.NameResolveResponse cid <- Ipfs.nameResolve ipnsId
    Ipfs.DagPutResponse newEntryCid <- Ipfs.dagPut $ MachineEntry values cid
    Ipfs.namePublish ipnsId $ Ipfs.AddressIpfs newEntryCid
    pure $ MachineEntryIndex newEntryCid

newtype Topic = Topic { getTopic :: Text }

-- | The IPFS pubsub topic associated to a machine.
machineTopic :: MachineId -> Topic
machineTopic (MachineId id) = Topic ("radicle:machine:" <> id)

-- | Publish a 'Message' on a machine's IPFS pubsub topic.
publish :: (MonadMachineIpfs m) => MachineId -> Message -> m ()
publish id msg = Ipfs.publish (getTopic (machineTopic id)) (Aeson.encode msg)

-- | Subscribe to messages on a machine's IPFS pubsub topic.
subscribeForever :: (MonadMachineIpfs m) => MachineId -> (Either Text Message -> IO ()) -> m ()
subscribeForever id messageHandler = Ipfs.subscribe topic pubsubHandler
  where
    Topic topic = machineTopic id
    pubsubHandler Ipfs.PubsubMessage{..} = messageHandler $ case decodeStrict messageData of
      Just msg -> Right msg
      Nothing  -> Left (toS messageData)

-- | Get inputs of an IPFS machine from least to most recent.
--
-- The returned 'Ipfs.MachineEntryIndex' is the index of the most
-- recent input.
--
-- If @maybeFrom@ is @Just index@ all inputs after (but not
-- including) @index@ are returned. Otherwise all inputs are returned.
machineInputsFrom
    :: forall m. (MonadMachineIpfs m)
    => MachineId -> Maybe MachineEntryIndex -> m (MachineEntryIndex, [Value])
machineInputsFrom (MachineId ipnsId) maybeFrom = do
    let MachineEntryIndex fromCid = fromMaybe (MachineEntryIndex emptyMachineCid) maybeFrom
    Ipfs.NameResolveResponse cid <- Ipfs.nameResolve ipnsId
    blocks <- getBlocks cid fromCid
    pure $ (MachineEntryIndex cid, blocks)
  where
    getBlocks :: CID -> CID -> m [Value]
    getBlocks cid fromCid = do
        if cid == fromCid || cid == emptyMachineCid
        then pure []
        else do
            let addr = Ipfs.AddressIpfs cid
            entry <- Ipfs.dagGet addr
            _ <- Ipfs.pinAdd addr
            rest <- getBlocks (entryPrevious entry) fromCid
            pure $ rest <> entryExpressions entry


-- | If a machine points to this ID then its log is considered empty.
-- The first entry in a machine log also points to this entry.
--
-- This is the CID produced by the document @{"radicle": true}@.
emptyMachineCid :: CID
emptyMachineCid =
    case cidFromText "zdpuAyyGtvC37aeZid2zh7LAGKCbFTn9MzdqoPpbNQm3BCwWT" of
        Left e    -> panic $ toS e
        Right cid -> cid


-- | Create an IPFS machine and return its ID.
--
-- Generates a new IPNS key and sets the IPNS record to the empty
-- machine DAG node.
createMachine :: (MonadMachineIpfs m) => m MachineId
createMachine = do
    uuid <- liftIO UUID.uuid
    Ipfs.KeyGenResponse ipnsId <- Ipfs.keyGen uuid
    Ipfs.namePublish ipnsId $ Ipfs.AddressIpfs emptyMachineCid
    pure $ MachineId ipnsId

-- | Publish to IPNS a machine entry index for a machine. Should only be issued
-- by the writer.
ipnsPublish :: (MonadMachineIpfs m) => MachineId -> MachineEntryIndex -> m ()
ipnsPublish id (MachineEntryIndex cid) = Ipfs.namePublish (getMachineId id) $ Ipfs.AddressIpfs cid

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
initSubscription :: (MonadMachineIpfs m) => MachineId -> (Text -> IO ()) -> m TopicSubscription
initSubscription id logParseError = do
    hdlrs <- liftIO $ newMVar Map.empty
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

emptyMachineEntryIndex :: MachineEntryIndex
emptyMachineEntryIndex = MachineEntryIndex emptyMachineCid
