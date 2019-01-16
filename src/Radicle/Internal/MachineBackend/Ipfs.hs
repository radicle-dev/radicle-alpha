-- | This module implements an IPFS machine backend.
--
-- Radicle Machines are identified by IPNS IDs. The IPNS ID of a
-- machine points to a linked list of expressions encoded as content
-- addressed IPLD documents and stored in IPFS’s DAG API. The nodes of
-- linked lists are represented by 'MachineEntry'. The corresponding
-- IPLD documents have the following shape.
-- @
--      {
--        "expressions": [
--          "(def foo :hey)",
--          "(get-value)"
--        ],
--        "previous": { "/": "QmA..." }
--      }
-- @
--
module Radicle.Internal.MachineBackend.Ipfs
    ( ipfsPrimFns
    , ipfsMachineCreate
    , IpnsId
    , ipfsMachineCreate
    ) where

import           Protolude hiding (TypeError, catch, try)

import           Control.Exception.Safe
import           Control.Monad.Fail
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.IPLD.CID
import qualified Data.Text as T

import           Radicle
import           Radicle.Internal.Core
import           Radicle.Internal.MachineBackend.Interface
import qualified Radicle.Internal.PrimFns as PrimFns
import qualified Radicle.Ipfs as Ipfs

-- | Primitive functions for the IPFS machine backend and the primitive
-- function @machine/ipfs/create!@.
ipfsPrimFns :: MonadIO m => PrimFns m
ipfsPrimFns =
    PrimFns.addPrimFn (unsafeToIdent createFnName) createFnDoc fn $
        buildMachineBackendPrimFns ipfsMachineBackend
  where
    createFnName = "machine/ipfs/create!"
    createFnDoc =
        "Create an IPFS machine and return its ID. Takes a local alias\
        \ for the machine as an argument. To avoid conflicts this should\
        \ be a UUID in production use."
    fn = PrimFns.oneArg createFnName $ \case
        String name ->
            liftIO (ipfsMachineCreate name) >>= \case
                Left err        -> throwErrorHere $ OtherError err
                Right machineId -> pure $ String machineId
        v -> throwErrorHere $ TypeError createFnName 0 TString v


-- | Create a Radicle machine and return its identifier. The argument
-- is the name to store with the returned key on the local. It is not
-- portable. To avoid conflicts it is recommended to use a UUID.
ipfsMachineCreate :: Text -> IO (Either Text Ipfs.IpnsId)
ipfsMachineCreate name = do
    res <- liftIO $ tryAny $ do
        Ipfs.KeyGenResponse machineId <- Ipfs.keyGen name
        Ipfs.namePublish machineId $ Ipfs.AddressIpfs emtpyMachineCid
        pure machineId
    pure $ first (toS . displayException) res


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
    Ipfs.NameResolveResponse cid <- Ipfs.nameResolve ipnsId
    Ipfs.DagPutResponse newEntryCid <- Ipfs.dagPut $ MachineEntry (toList values) cid
    Ipfs.namePublish ipnsId $ Ipfs.AddressIpfs newEntryCid
    pure $ MachineEntryIndex newEntryCid

receiveIpfs :: Ipfs.IpnsId -> Maybe MachineEntryIndex -> IO (MachineEntryIndex, [Value])
receiveIpfs ipnsId maybeFrom = do
    let MachineEntryIndex fromCid = fromMaybe (MachineEntryIndex emtpyMachineCid) maybeFrom
    Ipfs.NameResolveResponse cid <- Ipfs.nameResolve ipnsId
    blocks <- getBlocks cid fromCid
    pure $ (MachineEntryIndex cid, blocks)
  where
    getBlocks :: CID -> CID -> IO [Value]
    getBlocks cid fromCid = do
        if cid == fromCid || cid == emtpyMachineCid
        then pure []
        else do
            entry <- Ipfs.dagGet (Ipfs.AddressIpfs cid)
            rest <- getBlocks (entryPrevious entry) fromCid
            pure $ rest <> entryExpressions entry

-- | If a machine points to this ID then its log is considered empty.
-- The first entry in a machine log also points to this entry.
--
-- This is the CID produced by the document @{"radicle": true}@.
emtpyMachineCid :: CID
emtpyMachineCid =
    case cidFromText "zdpuAyyGtvC37aeZid2zh7LAGKCbFTn9MzdqoPpbNQm3BCwWT" of
        Left e    -> panic $ toS e
        Right cid -> cid

-- | A node in the linked list of expressions of a machine.
data MachineEntry = MachineEntry
    { entryExpressions :: [Value]
    , entryPrevious    :: CID
    } deriving (Eq, Show, Read, Generic)

instance FromJSON MachineEntry where
    parseJSON = Aeson.withObject "MachineEntry" $ \o -> do
        expressionCode <- o .: "expression"
        let src = "[ipfs]"
        entryExpressions <-
            case parseValues src expressionCode of
                Left err  -> fail $ "failed to parse Radicle expression: " <> show err
                Right v -> pure v
        entryPrevious <- Ipfs.parseIpldLink =<< o .: "previous"
        pure MachineEntry {..}

instance ToJSON MachineEntry where
    toJSON MachineEntry{..} =
        let code = T.intercalate "\n" $ map renderCompactPretty entryExpressions
        in Aeson.object
            [ "expression" .= code
            , "previous" .= Ipfs.ipldLink entryPrevious
            ]
