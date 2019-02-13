module Radicle.Daemon.Ipfs
  ( IpfsError(..)
  , MachineId(..)
  , Message(..)
  , InputsApplied(..)
  , SubmitInputs(..)
  , writeIpfs
  , publish
  , machineInputsFrom
  , createMachine
  , Ipfs.MachineEntryIndex
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Unique as Unique
import qualified Network.HTTP.Client as Http

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

data IpfsError
  = IpfsDaemonException Ipfs.IpfsException
  | IpfsDaemonErrMsg Text
  | IpfsDaemonNoErrMsg
  | Timeout
--  | InternalError Text

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

-- TODO: All the error handling w.r.t interactions with IPFS should be
-- done in "Radicle.Ipfs".
safeIpfs :: forall a. IO a -> ExceptT IpfsError IO a
safeIpfs io = ExceptT $ liftIO $
  catches (Right <$> io)
    [ Handler $ pure . Left . IpfsDaemonException
    , Handler httpHdlr
    ]
  where
    httpHdlr e = pure $ Left $ case e of
      Http.HttpExceptionRequest _
        (Http.StatusCodeException _
          (decodeStrict ->
             Just (Aeson.Object (HashMap.lookup "Message" ->
                     Just (Aeson.String msg))))) -> IpfsDaemonErrMsg msg
      Http.HttpExceptionRequest _ Http.ResponseTimeout -> Timeout
      _ -> IpfsDaemonNoErrMsg


-- | Write and pin some inputs to an IPFS machine.
-- TODO: we might want to make this safer by taking the expected head
-- MachineEntryIndex as an argument.
writeIpfs :: MachineId -> [Value] -> ExceptT IpfsError IO Ipfs.MachineEntryIndex
writeIpfs (MachineId id) vs = safeIpfs $ Ipfs.sendIpfs id (Seq.fromList vs)

newtype Topic = Topic { getTopic :: Text }

-- | The IPFS pubsub topic associated to a machine.
machineTopic :: MachineId -> Topic
machineTopic (MachineId id) = Topic ("radicle:machine:" <> id)

-- | Publish a 'Message' on a machine's IPFS pubsub topic.
publish :: MachineId -> Message -> ExceptT IpfsError IO ()
publish id msg = safeIpfs $
  Ipfs.publish (getTopic (machineTopic id)) (Aeson.encode msg)

-- | Subscribe to messages on a machine's IPFS pubsub topic.
subscribeForever :: MachineId -> MsgHandler -> IO ()
subscribeForever id messageHandler = Ipfs.subscribe topic pubsubHandler
  where
    Topic topic = machineTopic id
    pubsubHandler Ipfs.PubsubMessage{..} = messageHandler $ case decodeStrict messageData of
      Just msg -> Right msg
      Nothing  -> Left (toS messageData)

-- | Get and pin inputs of an IPFS machine from a certain index.
machineInputsFrom :: MachineId -> Maybe Ipfs.MachineEntryIndex -> ExceptT IpfsError IO (Ipfs.MachineEntryIndex, [Value])
machineInputsFrom (MachineId id) = safeIpfs . Ipfs.receiveIpfs id

-- | Create an IPFS machine and return its ID.
createMachine :: ExceptT IpfsError IO MachineId
createMachine = do
  id_ <- safeIpfs $ second MachineId <$> (Ipfs.ipfsMachineCreate =<< UUID.uuid)
  case id_ of
    Left e   -> throwError (IpfsDaemonErrMsg e)
    Right id -> pure id

-- * Topic subscriptions

type MsgHandler = Either Text Message -> IO ()

type RegisteredHandler = Unique.Unique

newtype TopicSubscription = TopicSubscription
  { handlers :: MVar (Map RegisteredHandler MsgHandler)
  }

-- | Given a machine ID, creates a subscription for that machine's
-- pubsub topic. This allows adding and removing handlers for messages
-- on that topic.
initSubscription :: MachineId -> ExceptT IpfsError IO TopicSubscription
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
subscribeOne :: TopicSubscription -> Int64 -> (Message -> Bool) -> (Text -> IO ()) -> IO (Maybe Message)
subscribeOne sub timeout pr badMsg = do
  var <- newEmptyMVar
  let onMsg = \case
        Right msg | pr msg -> putMVar var (Just msg)
        Right _ -> pure ()
        Left bad -> badMsg bad
  bracket
    (addHandler sub onMsg)
    (removeHandler sub)
    (const $ forkIO (threadDelay (fromIntegral timeout * 1000) >> putMVar var Nothing) *> readMVar var)
