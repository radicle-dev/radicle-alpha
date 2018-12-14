module Radicle.Internal.MachineBackend.Ipfs
    ( ipfsPrimFns
    , ipfsMachineBackend
    ) where

import           Protolude hiding (TypeError, catch, try)

import           Control.Exception.Safe
import           Control.Lens ((.~), (^.))
import           Control.Monad.Fail
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.IPLD.CID
import qualified Data.Text as T
import           Network.HTTP.Client
                 (HttpException(..), HttpExceptionContent(..))
import qualified Network.Wreq as Wreq

import           Radicle
import           Radicle.Internal.Core
import           Radicle.Internal.MachineBackend.Interface
import qualified Radicle.Internal.PrimFns as PrimFns

ipfsPrimFns :: MonadIO m => PrimFns m
ipfsPrimFns =
    -- TODO doc
    PrimFns.addPrimFn (unsafeToIdent createFnName) "TODO doc" fn $
        buildMachineBackendPrimFns ipfsMachineBackend
  where
    createFnName = "machine/ipfs/create!"
    fn = PrimFns.oneArg createFnName $ \case
        String name ->
            liftIO (ipfsMachineCreate name) >>= \case
                Left err        -> throwErrorHere $ OtherError err
                Right machineId -> pure $ String machineId
        v -> throwErrorHere $ TypeError createFnName 0 TString v


-- TODO doc
ipfsMachineCreate :: Text -> IO (Either Text IpnsId)
ipfsMachineCreate name = do
    res <- liftIO $ tryAny $ do
        IpfsKeyGenResponse machineId <- ipfsKeyGen name
        namePublish machineId $ IpfsAddressIpfs firstEntryCid
        pure machineId
    pure $ first (toS . displayException) res


data IpfsException
    = IpfsException Text
    -- | JSON response from the IPFS Api cannot be parsed. First
    -- argument is the request path, second argument the JSON parsing
    -- error
    | IpfsExceptionInvalidResponse Text Text
    | IpfsExceptionNoDaemon
    deriving (Show, Eq)

instance Exception IpfsException where
    displayException (IpfsException msg) = "ipfs: " <> toS msg
    displayException IpfsExceptionNoDaemon = "ipfs: Daemon not reachable, run 'rad ipfs start'"
    displayException (IpfsExceptionInvalidResponse url _) = "ipfs: Cannot parse IPFS daemon response for " <> toS url


newtype MachineEntryIndex = MachineEntryIndex CID
    deriving (Show, Eq)

instance (CPA t) => ToRad t MachineEntryIndex where
    toRad (MachineEntryIndex cid) = String $ cidToText cid

instance (CPA t) => FromRad t MachineEntryIndex where
    fromRad (String cid) =
        case cidFromText cid of
            Left err   -> Left $ toS err
            Right cid' -> Right $ MachineEntryIndex cid'
    fromRad _ = Left "IPFS entry index must be a string"


ipfsMachineBackend :: MonadIO m => MachineBackend MachineEntryIndex m
ipfsMachineBackend =
    MachineBackend
        { machineType = "ipfs"
        , machineUpdate =
            \id values -> do
                res <- liftIO $ tryAny $ sendIpfs id values
                pure $ first (toS . displayException) res
        , machineGetLog =
            \id maybeFrom -> do
                res <- liftIO $ tryAny $ receiveIpfs id maybeFrom
                pure $ first (toS . displayException) res
        }

sendIpfs :: Text -> Seq Value -> IO MachineEntryIndex
sendIpfs ipnsId values = do
    IpfsNameResolveResponse cid <- nameResolve ipnsId
    IpfsDagPutResponse newEntryCid <- dagPut $ MachineEntry (toList values) cid
    namePublish ipnsId $ IpfsAddressIpfs newEntryCid
    pure $ MachineEntryIndex newEntryCid

receiveIpfs :: IpnsId -> Maybe MachineEntryIndex -> IO (MachineEntryIndex, [Value])
receiveIpfs ipnsId maybeFrom = do
    let MachineEntryIndex fromCid = fromMaybe (MachineEntryIndex firstEntryCid) maybeFrom
    IpfsNameResolveResponse cid <- nameResolve ipnsId
    blocks <- getBlocks cid fromCid
    pure $ (MachineEntryIndex cid, blocks)
  where
    getBlocks :: CID -> CID -> IO [Value]
    getBlocks cid fromCid = do
        if cid == fromCid || cid == firstEntryCid
        then pure []
        else do
            entry <- dagGet (IpfsAddressIpfs cid)
            rest <- getBlocks (entryParent entry) fromCid
            pure $ rest <> entryExpressions entry

-- TODO doc
firstEntryCid :: CID
firstEntryCid =
    case cidFromText "zdpuAyyGtvC37aeZid2zh7LAGKCbFTn9MzdqoPpbNQm3BCwWT" of
        Left e    -> panic $ toS e
        Right cid -> cid

-- TODO doc
data MachineEntry = MachineEntry
    { entryExpressions :: [Value]
    , entryParent      :: CID
    } deriving (Eq, Show, Read, Generic)

instance FromJSON MachineEntry where
    parseJSON = Aeson.withObject "MachineEntry" $ \o -> do
        expressionCode <- o .: "expression"
        let src = "[ipfs]"
        entryExpressions <-
            case parseValues src expressionCode of
                Left err  -> fail $ "failed to parse Radicle expression: " <> show err
                Right v -> pure v
        entryParent <- parseIpldLink =<< o .: "parent"
        pure MachineEntry {..}

instance ToJSON MachineEntry where
    toJSON MachineEntry{..} =
        let code = T.intercalate "\n" $ map renderCompactPretty entryExpressions
        in Aeson.object
            [ "expression" .= code
            , "parent" .= ipldLink entryParent
            ]

ipldLink :: CID -> Aeson.Value
ipldLink cid = Aeson.object [ "/" .= cidToText cid ]

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

data IpfsAddress
    = IpfsAddressIpfs CID
    | IpfsAddressIpns IpnsId
    deriving (Eq, Show, Read, Generic)

-- This is the same representation of IPFS paths as used by the IPFS CLI and
-- daemon.
ipfsPathToText :: IpfsAddress -> Text
ipfsPathToText (IpfsAddressIpfs cid)    = "/ipfs/" <> cidToText cid
ipfsPathToText (IpfsAddressIpns ipnsId) = "/ipns/" <> ipnsId

-- | Partial inverse of 'ipfsPathToText'.
ipfsPathFromText :: Text -> Maybe IpfsAddress
ipfsPathFromText t =
        (IpfsAddressIpfs <$> maybeIpfsPath)
    <|> (IpfsAddressIpns <$> T.stripPrefix "/ipns/" t)

  where
    maybeIpfsPath = do
        cidText <- T.stripPrefix "/ipfs/" t
        case cidFromText cidText of
            Left _    -> Nothing
            Right cid -> Just cid


--------------------------------------------------------------------------
-- * IPFS node API
--------------------------------------------------------------------------

newtype IpfsKeyGenResponse = IpfsKeyGenResponse IpnsId

ipfsKeyGen :: Text -> IO IpfsKeyGenResponse
ipfsKeyGen name = ipfsHttpGet "key/gen" [("arg", name), ("type", "ed25519")]

instance FromJSON IpfsKeyGenResponse where
    parseJSON = Aeson.withObject "ipfs key/gen" $ \o ->
        IpfsKeyGenResponse <$> o .: "Id"


newtype IpfsDagPutResponse
    = IpfsDagPutResponse CID

dagPut :: ToJSON a => a -> IO IpfsDagPutResponse
dagPut obj = ipfsHttpPost "dag/put?pin=true" "arg" (Aeson.encode obj)

instance FromJSON IpfsDagPutResponse where
    parseJSON = Aeson.withObject "v0/dag/put response" $ \o -> do
        cidObject <- o .: "Cid"
        cidText <- cidObject .: "/"
        case cidFromText cidText of
            Left _    -> fail "invalid CID"
            Right cid -> pure $ IpfsDagPutResponse cid


dagGet :: FromJSON a => IpfsAddress -> IO a
dagGet addr = do
    result <- ipfsHttpGet "dag/get" [("arg", ipfsPathToText addr)]
    case Aeson.fromJSON result of
        Aeson.Error _   -> throw $ IpfsException $ "Invalid machine log entry at " <> ipfsPathToText addr
        Aeson.Success a -> pure a

namePublish :: IpnsId -> IpfsAddress -> IO ()
namePublish ipnsId addr = do
    _ :: Aeson.Value <- ipfsHttpGet "name/publish" [("arg", ipfsPathToText addr), ("key", ipnsId)]
    pure ()


newtype IpfsNameResolveResponse
    = IpfsNameResolveResponse CID

nameResolve :: IpnsId -> IO IpfsNameResolveResponse
nameResolve ipnsId = ipfsHttpGet "name/resolve" [("arg", ipnsId), ("recursive", "true")]

instance FromJSON IpfsNameResolveResponse where
    parseJSON = Aeson.withObject "v0/name/resolve response" $ \o -> do
        path <- o .: "Path"
        case ipfsPathFromText path of
            Nothing                    -> fail "invalid IPFS path"
            Just (IpfsAddressIpfs cid) -> pure $ IpfsNameResolveResponse cid
            Just _                     -> fail "expected /ipfs path"


--------------------------------------------------------------------------
-- * IPFS Internal
--------------------------------------------------------------------------

ipfsHttpGet
    :: FromJSON a
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> [(Text, Text)] -- ^ URL query parameters
    -> IO a
ipfsHttpGet path params = do
    let url = "http://localhost:9301/api/v0/" <> path
    let opts = Wreq.defaults & Wreq.params .~ params
    res <- Wreq.getWith opts (toS url) `catch` handleRequestException
    jsonRes <- Wreq.asJSON res `catch` handleParseException path
    pure $ jsonRes ^. Wreq.responseBody

ipfsHttpPost
    :: FromJSON a
    => Text  -- ^ Path of the endpoint under "/api/v0/"
    -> Text  -- ^ Name of the argument for payload
    -> LByteString -- ^ Payload argument
    -> IO a
ipfsHttpPost path payloadArgName payload = do
    let url = "http://localhost:9301/api/v0/" <> path
    res <- Wreq.post (toS url) (Wreq.partLBS payloadArgName payload) `catch` handleRequestException
    jsonRes <- Wreq.asJSON res `catch` handleParseException path
    pure $ jsonRes ^. Wreq.responseBody

handleRequestException :: MonadThrow m => HttpException -> m a
handleRequestException (HttpExceptionRequest _ (ConnectionFailure _)) =
    throw IpfsExceptionNoDaemon
handleRequestException ex = throw ex

handleParseException :: MonadCatch m => Text -> Wreq.JSONError -> m a
handleParseException path (Wreq.JSONError msg) = throw $ IpfsExceptionInvalidResponse path (toS msg)
