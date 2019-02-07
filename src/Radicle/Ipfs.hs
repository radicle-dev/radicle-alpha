-- | Functions to talk to the IPFS API
module Radicle.Ipfs
    ( IpfsException(..)
    , ipldLink
    , parseIpldLink
    , IpnsId
    , Address(..)
    , addressToText
    , addressFromText

    , KeyGenResponse(..)
    , keyGen

    , DagPutResponse(..)
    , dagPut
    , dagGet

    , namePublish
    , NameResolveResponse(..)
    , nameResolve

    , PubsubMessage(..)
    , publish
    , subscribe
    ) where

import           Protolude hiding (TypeError, catch, try)

import           Control.Exception.Safe
import           Control.Monad.Fail
import           Control.Monad.Trans.Resource
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Multibase as Multibase
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Combinators as C
import           Data.IPLD.CID
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Lens.Micro ((.~), (^.))
import           Network.HTTP.Client
                 (HttpException(..), HttpExceptionContent(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.Wreq as Wreq
import           System.Environment (lookupEnv)

data IpfsException
    = IpfsException Text
    -- | JSON response from the IPFS Api cannot be parsed. First
    -- argument is the request path, second argument the JSON parsing
    -- error
    | IpfsExceptionInvalidResponse Text Text
    -- | The IPFS daemon is not running.
    | IpfsExceptionNoDaemon
    deriving (Show, Eq)

instance Exception IpfsException where
    displayException (IpfsException msg) = "ipfs: " <> toS msg
    displayException IpfsExceptionNoDaemon = "ipfs: Daemon not reachable, run 'rad ipfs daemon"
    displayException (IpfsExceptionInvalidResponse url _) = "ipfs: Cannot parse IPFS daemon response for " <> toS url



-- | Given a CID @"abc...def"@ it returns a IPLD link JSON object
-- @{"/": "abc...def"}@.
ipldLink :: CID -> Aeson.Value
ipldLink cid = Aeson.object [ "/" .= cidToText cid ]

-- | Parses JSON values of the form @{"/": "abc...def"}@ where
-- @"abc...def"@ is a valid CID.
parseIpldLink :: Aeson.Value -> Aeson.Parser CID
parseIpldLink =
    Aeson.withObject "IPLD link" $ \o -> do
        cidText <- o .: "/"
        case cidFromText cidText of
            Left e    -> fail $ "Invalid CID: " <> e
            Right cid -> pure cid

--------------------------------------------------------------------------
-- * IPFS types
--------------------------------------------------------------------------

type IpnsId = Text

-- | Addresses either an IPFS content ID or an IPNS ID.
data Address
    = AddressIpfs CID
    | AddressIpns IpnsId
    deriving (Eq, Show, Read, Generic)

-- This is the same representation of IPFS paths as used by the IPFS CLI and
-- daemon. Either @"/ipfs/abc...def"@ or @"/ipns/abc...def"@.
addressToText :: Address -> Text
addressToText (AddressIpfs cid)    = "/ipfs/" <> cidToText cid
addressToText (AddressIpns ipnsId) = "/ipns/" <> ipnsId

-- | Partial inverse of 'addressToText'.
addressFromText :: Text -> Maybe Address
addressFromText t =
        (AddressIpfs <$> maybeAddress)
    <|> (AddressIpns <$> T.stripPrefix "/ipns/" t)
  where
    maybeAddress = do
        cidText <- T.stripPrefix "/ipfs/" t
        case cidFromText cidText of
            Left _    -> Nothing
            Right cid -> Just cid


--------------------------------------------------------------------------
-- * IPFS node API
--------------------------------------------------------------------------

data PubsubMessage = PubsubMessage
    { messageTopicIDs :: [Text]
    , messageData     :: ByteString
    , messageFrom     :: ByteString
    , messageSeqno    :: ByteString
    } deriving (Eq, Show)

instance FromJSON PubsubMessage where
    parseJSON = Aeson.withObject "PubsubMessage" $ \o -> do
        messageTopicIDs <- o .: "topicIDs"
        Right messageData <- o .: "data" <&> T.encodeUtf8 <&> Multibase.decodeBase64
        Right messageFrom <- o .: "from" <&> T.encodeUtf8 <&> Multibase.decodeBase64
        Right messageSeqno <- o .: "seqno" <&> T.encodeUtf8 <&> Multibase.decodeBase64
        pure PubsubMessage {..}

-- | Subscribe to a topic and call @messageHandler@ on every message.
-- The IO action blocks while we are subscribed. To stop subscription
-- you need to kill the thread the subscription is running in.
subscribe :: Text -> (PubsubMessage -> IO ()) -> IO ()
subscribe topic messageHandler = runResourceT $ do
    mgr <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
    req <- HTTP.parseRequest "http://localhost:9301/api/v0/pubsub/sub" <&>
        HTTP.setQueryString
        [ ("arg", Just $ T.encodeUtf8 topic)
        , ("encoding", Just "json")
        , ("stream-channels", Just "true")
        ]
    body <- HTTP.responseBody <$> HTTP.http req mgr
    C.runConduit $ body .| fromJSONC .| C.mapM_ (liftIO . messageHandler) .| C.sinkNull
    pure ()
  where
    fromJSONC :: (MonadThrow m, Aeson.FromJSON a) => C.ConduitT ByteString a m ()
    fromJSONC = jsonC .| C.mapM parseThrow

    jsonC :: (MonadThrow m) => C.ConduitT ByteString Aeson.Value m ()
    jsonC = C.peekForever (C.sinkParser Aeson.json >>= C.yield)

    parseThrow :: (MonadThrow m, Aeson.FromJSON a) => Aeson.Value -> m a
    parseThrow value = do
        case Aeson.fromJSON value of
            Aeson.Error err -> throwString err
            Aeson.Success a -> pure a


-- | Publish a message to a topic.
publish :: Text -> LByteString -> IO ()
publish topic message =
    void $ ipfsHttpPost' "pubsub/pub" [("arg", topic)] "data" message

newtype KeyGenResponse = KeyGenResponse IpnsId

keyGen :: Text -> IO KeyGenResponse
keyGen name = ipfsHttpGet "key/gen" [("arg", name), ("type", "ed25519")]

instance FromJSON KeyGenResponse where
    parseJSON = Aeson.withObject "ipfs key/gen" $ \o ->
        KeyGenResponse <$> o .: "Id"


newtype DagPutResponse
    = DagPutResponse CID

-- | Put and pin a dag node.
dagPut :: ToJSON a => a -> IO DagPutResponse
dagPut obj = ipfsHttpPost "dag/put" [("pin", "true")] "arg" (Aeson.encode obj)

instance FromJSON DagPutResponse where
    parseJSON = Aeson.withObject "v0/dag/put response" $ \o -> do
        cidObject <- o .: "Cid"
        cidText <- cidObject .: "/"
        case cidFromText cidText of
            Left _    -> fail "invalid CID"
            Right cid -> pure $ DagPutResponse cid

newtype PinResponse = PinResponse [CID]

instance FromJSON PinResponse where
  parseJSON = Aeson.withObject "v0/pin/add response" $ \o -> do
    cidTexts <- o .: "Pins"
    case traverse cidFromText cidTexts of
      Left _     -> fail "invalid CID"
      Right cids -> pure $ PinResponse cids

-- | Get and pin a DAG node.
dagGet :: FromJSON a => Address -> IO a
dagGet addr = do
    let params = [("arg", addressToText addr)]
    result <- ipfsHttpGet "dag/get" params
    PinResponse _ <- ipfsHttpGet "pin/add" params
    case Aeson.fromJSON result of
        Aeson.Error _   -> throw $ IpfsException $ "Invalid machine log entry at " <> addressToText addr
        Aeson.Success a -> pure a

namePublish :: IpnsId -> Address -> IO ()
namePublish ipnsId addr = do
    _ :: Aeson.Value <- ipfsHttpGet "name/publish" [("arg", addressToText addr), ("key", ipnsId)]
    pure ()


newtype NameResolveResponse
    = NameResolveResponse CID

nameResolve :: IpnsId -> IO NameResolveResponse
nameResolve ipnsId = ipfsHttpGet "name/resolve" [("arg", ipnsId), ("recursive", "true")]

instance FromJSON NameResolveResponse where
    parseJSON = Aeson.withObject "v0/name/resolve response" $ \o -> do
        path <- o .: "Path"
        case addressFromText path of
            Nothing                -> fail "invalid IPFS path"
            Just (AddressIpfs cid) -> pure $ NameResolveResponse cid
            Just _                 -> fail "expected /ipfs path"


--------------------------------------------------------------------------
-- * IPFS Internal
--------------------------------------------------------------------------

ipfsHttpGet
    :: FromJSON a
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> IO a
ipfsHttpGet path params = do
    let opts = Wreq.defaults & Wreq.params .~ params
    url <- ipfsApiUrl path
    res <- Wreq.getWith opts (toS url) `catch` handleRequestException
    jsonRes <- Wreq.asJSON res `catch` handleParseException path
    pure $ jsonRes ^. Wreq.responseBody

ipfsHttpPost
    :: FromJSON a
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> Text  -- ^ Name of the argument for payload
    -> LByteString -- ^ Payload argument
    -> IO a
ipfsHttpPost path params payloadArgName payload = do
    res <- ipfsHttpPost' path params payloadArgName payload
    jsonRes <- Wreq.asJSON res `catch` handleParseException path
    pure $ jsonRes ^. Wreq.responseBody

ipfsHttpPost'
    :: Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> Text  -- ^ Name of the argument for payload
    -> LByteString -- ^ Payload argument
    -> IO (Wreq.Response LByteString)
ipfsHttpPost' path params payloadArgName payload = do
    let opts = Wreq.defaults & Wreq.params .~ params
    url <- ipfsApiUrl path
    Wreq.postWith opts (toS url) (Wreq.partLBS payloadArgName payload) `catch` handleRequestException

ipfsApiUrl :: Text -> IO Text
ipfsApiUrl path = do
    baseUrl <- fromMaybe "http://localhost:9301" <$> lookupEnv "RAD_IPFS_API_URL"
    pure $ toS baseUrl <> "/api/v0/" <> path

handleRequestException :: MonadThrow m => HttpException -> m a
handleRequestException (HttpExceptionRequest _ (ConnectionFailure _)) =
    throw IpfsExceptionNoDaemon
handleRequestException ex = throw ex

handleParseException :: MonadCatch m => Text -> Wreq.JSONError -> m a
handleParseException path (Wreq.JSONError msg) = throw $ IpfsExceptionInvalidResponse path (toS msg)
