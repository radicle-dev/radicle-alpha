-- | Functions to talk to the IPFS API
module Radicle.Ipfs
    ( MonadIpfs(..)
    , Client
    , newIpfsHttpClientManager
    , newClient

    , IpfsException(..)
    , ipldLink
    , parseIpldLink
    , IpnsId
    , Address(..)
    , addressToText
    , addressFromText

    , VersionResponse(..)
    , version

    , KeyGenResponse(..)
    , keyGen

    , DagPutResponse(..)
    , dagPut
    , dagGet
    , pinAdd

    , namePublish
    , NameResolveResponse(..)
    , nameResolve

    , PubsubMessage(..)
    , publish
    , subscribe
    ) where

import           Protolude hiding (TypeError, catch, catches, try)

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
import qualified Data.HashMap.Strict as HashMap
import           Data.IPLD.CID
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Client
                 (HttpException(..), HttpExceptionContent(..))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.MultipartFormData as HttpClient
import qualified Network.HTTP.Conduit as HttpConduit
import qualified Network.HTTP.Types.Method as Http
import           System.Environment (lookupEnv)

class (MonadIO m, MonadCatch m) => MonadIpfs m where
    askClient :: m Client

data Client = Client
    { clientBaseRequest :: HttpClient.Request
    , clientHttpManager :: HttpClient.Manager
    }

newIpfsHttpClientManager :: IO HttpClient.Manager
newIpfsHttpClientManager = HttpClient.newManager
  HttpClient.defaultManagerSettings{
    HttpClient.managerResponseTimeout = HttpClient.responseTimeoutMicro (ipfsTimeoutSeconds * 1000000)
  }

newClient :: MonadIO m => m Client
newClient = liftIO $ do
    baseUrl <- fromMaybe "http://localhost:9301" <$> lookupEnv "RAD_IPFS_API_URL"
    clientBaseRequest <- HttpClient.parseUrlThrow baseUrl
    clientHttpManager <- newIpfsHttpClientManager
    pure Client{clientBaseRequest, clientHttpManager}

data IpfsException
  = IpfsException Text
  | IpfsExceptionErrResp Text
  | IpfsExceptionErrRespNoMsg
  -- | The request to the IPFS daemon timed out. The constructor
  -- parameter is the API path.
  | IpfsExceptionTimeout Text
  -- | JSON response from the IPFS Api cannot be parsed. First
  -- argument is the request path, second argument the JSON parsing
  -- error
  | IpfsExceptionInvalidResponse Text Text
  -- | The IPFS daemon is not running.
  | IpfsExceptionNoDaemon
  -- | Failed to parse IPLD document returned by @dag/get@ with
  -- 'Aeson.fromJSON'. First argument is the IPFS address, second argument is
  -- the Aeson parse error.
  | IpfsExceptionIpldParse Address Text
  deriving (Show)

instance Exception IpfsException where
    displayException e = "ipfs: " <> case e of
        IpfsException msg -> toS msg
        IpfsExceptionNoDaemon -> "Cannot connect to " <> name
        IpfsExceptionInvalidResponse url _ -> "Cannot parse " <> name <> " response for " <> toS url
        IpfsExceptionTimeout apiPath -> name <> " took too long to respond for " <> toS apiPath
        IpfsExceptionErrResp msg -> toS msg
        IpfsExceptionErrRespNoMsg -> name <> " failed with no error message"
        IpfsExceptionIpldParse addr parseError ->
            toS $ "Failed to parse IPLD document " <> addressToText addr <> ": " <> parseError
      where
        name = "Radicle IPFS daemon"

-- | Catches 'HttpException's and re-throws them as 'IpfsException's.
--
-- @path@ is the IPFS API path that is added to some errors.
mapHttpException :: (MonadCatch m) => Text -> m a -> m a
mapHttpException path io = catch io (throw . mapHttpExceptionData)
  where
    mapHttpExceptionData :: HttpException -> IpfsException
    mapHttpExceptionData = \case
        HttpClient.HttpExceptionRequest _ content -> mapHttpExceptionContent content
        _ -> IpfsExceptionErrRespNoMsg

    mapHttpExceptionContent :: HttpExceptionContent -> IpfsException
    mapHttpExceptionContent = \case
        (HttpClient.StatusCodeException _
          (Aeson.decodeStrict ->
             Just (Aeson.Object (HashMap.lookup "Message" ->
                     Just (Aeson.String msg))))) -> (IpfsExceptionErrResp msg)
        HttpClient.ResponseTimeout -> IpfsExceptionTimeout path
        ConnectionFailure _ -> IpfsExceptionNoDaemon
        _ -> IpfsExceptionErrRespNoMsg

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

newtype VersionResponse = VersionResponse Text

instance FromJSON VersionResponse where
    parseJSON = Aeson.withObject "VersionResponse" $ \o -> do
        v <- o .: "Version"
        pure $ VersionResponse v

version :: (MonadIpfs m) => m VersionResponse
version = ipfsHttpGet "version" []

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
subscribe :: (MonadIpfs m) => Text -> (PubsubMessage -> IO ()) -> m ()
subscribe topic messageHandler = do
    Client{clientHttpManager, clientBaseRequest} <- askClient
    liftIO $ runResourceT $ do
        let req = makeIpfsRequest
                    clientBaseRequest
                    Http.methodGet
                    "pubsub/sub"
                    [ ("arg", topic)
                    , ("encoding", "json")
                    , ("stream-channels", "true")
                    ]
        body <- HttpConduit.responseBody <$> HttpConduit.http req clientHttpManager
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
publish :: (MonadIpfs m) =>  Text -> LByteString -> m ()
publish topic message =
     void $ ipfsHttpPost' "pubsub/pub" [("arg", topic)] "data" message

newtype KeyGenResponse = KeyGenResponse IpnsId

keyGen :: (MonadIpfs m) => Text -> m KeyGenResponse
keyGen name = ipfsHttpGet "key/gen" [("arg", name), ("type", "ed25519")]

instance FromJSON KeyGenResponse where
    parseJSON = Aeson.withObject "ipfs key/gen" $ \o ->
        KeyGenResponse <$> o .: "Id"


newtype DagPutResponse
    = DagPutResponse CID

-- | Put and pin a dag node.
dagPut :: (MonadIpfs m, ToJSON a) => a -> m DagPutResponse
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

-- | Pin objects to local storage.
pinAdd :: (MonadIpfs m) => Address -> m PinResponse
pinAdd addr = ipfsHttpGet "pin/add" [("arg", addressToText addr)]

-- | Get a dag node.
dagGet :: (MonadIpfs m) => FromJSON a => Address -> m a
dagGet addr = do
    result <- ipfsHttpGet "dag/get" [("arg", addressToText addr)]
    case Aeson.fromJSON result of
        Aeson.Error err -> throw $ IpfsExceptionIpldParse addr (toS err)
        Aeson.Success a -> pure a

namePublish :: (MonadIpfs m) => IpnsId -> Address -> m ()
namePublish ipnsId addr = do
    _ :: Aeson.Value <- ipfsHttpGet "name/publish" [("arg", addressToText addr), ("key", ipnsId)]
    pure ()


newtype NameResolveResponse
    = NameResolveResponse CID

nameResolve :: (MonadIpfs m) => IpnsId -> m NameResolveResponse
nameResolve ipnsId = ipfsHttpGet "name/resolve" [("arg", ipnsId), ("recursive", "true"), ("dht-timeout", show ipfsTimeoutSeconds <> "s")]

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
    :: (FromJSON a, MonadIpfs m)
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> m a
ipfsHttpGet path params = do
    Client{clientHttpManager, clientBaseRequest} <- askClient
    liftIO $ mapHttpException path $ do
        let request = makeIpfsRequest clientBaseRequest Http.methodGet path params
        response <- HttpClient.httpLbs request clientHttpManager
        parseJsonResponse path $ HttpClient.responseBody response

ipfsHttpPost
    :: (MonadIpfs m, FromJSON a)
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> Text  -- ^ Name of the argument for payload
    -> LByteString -- ^ Payload argument
    -> m a
ipfsHttpPost path params payloadArgName payload = do
    res <- ipfsHttpPost' path params payloadArgName payload
    parseJsonResponse path res

ipfsHttpPost'
    :: (MonadIpfs m)
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> Text  -- ^ Name of the argument for payload
    -> LByteString -- ^ Payload argument
    -> m LByteString
ipfsHttpPost' path params payloadArgName payload = do
    Client{clientHttpManager, clientBaseRequest} <- askClient
    liftIO $ mapHttpException path $ do
        let request = makeIpfsRequest clientBaseRequest Http.methodPost path params
        let part = HttpClient.partLBS payloadArgName payload
        requestWithPayload <- HttpClient.formDataBody [part] request
        response <- HttpClient.httpLbs requestWithPayload clientHttpManager
        pure $ HttpClient.responseBody response


makeIpfsRequest
    :: HttpClient.Request -- ^ Base request that contains the base URL
    -> Http.Method
    -> Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> HttpClient.Request
makeIpfsRequest baseRequest method path params =
    HttpClient.setQueryString params'
        baseRequest
            { HttpClient.path = toS $ "/api/v0/" <> path
            , HttpClient.method = method
            }
  where
    params' = [ (encodeUtf8 key, Just (encodeUtf8 value)) | (key, value) <- params]

-- | Timeout for IPFS API.
ipfsTimeoutSeconds :: Int
ipfsTimeoutSeconds = 60

-- | Parses response body as JSON and returns the parsed value. @path@
-- is the IPFS API the response was obtained from. Throws
-- 'IpfsExceptionInvalidResponse' if parsing fails.
parseJsonResponse :: (MonadThrow m, FromJSON a) => Text -> LByteString -> m a
parseJsonResponse path body = do
    case Aeson.eitherDecode' body of
        Left err -> throw $ IpfsExceptionInvalidResponse path (toS err)
        Right a  -> pure a
