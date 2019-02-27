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
module Radicle.Internal.MachineBackend.Ipfs where

import           Protolude hiding (catch, try)

import           Control.Exception.Safe
import           Control.Monad.Fail
import           Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.IPLD.CID

import           Radicle.Internal.Core
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import qualified Radicle.Ipfs as Ipfs

-- | Create a Radicle machine and return its identifier. The argument
-- is the name to store with the returned key on the local. It is not
-- portable. To avoid conflicts it is recommended to use a UUID.
ipfsMachineCreate :: Text -> IO (Either Text Ipfs.IpnsId)
ipfsMachineCreate name = do
    res <- liftIO $ tryAny $ do
        Ipfs.KeyGenResponse machineId <- Ipfs.keyGen name
        Ipfs.namePublish machineId $ Ipfs.AddressIpfs emptyMachineCid
        pure machineId
    pure $ first (toS . displayException) res


newtype MachineEntryIndex = MachineEntryIndex CID
    deriving (Show, Eq)

instance Aeson.ToJSON MachineEntryIndex where
  toJSON (MachineEntryIndex cid) = Ipfs.ipldLink cid

instance Aeson.FromJSON MachineEntryIndex where
  parseJSON = fmap MachineEntryIndex . Ipfs.parseIpldLink

-- | Write and pin a sequence of expressions to a machine.
sendIpfs :: Ipfs.IpnsId -> Seq Value -> IO MachineEntryIndex
sendIpfs ipnsId values = do
    Ipfs.NameResolveResponse cid <- Ipfs.nameResolve ipnsId
    Ipfs.DagPutResponse newEntryCid <- Ipfs.dagPut $ MachineEntry (toList values) cid
    Ipfs.namePublish ipnsId $ Ipfs.AddressIpfs newEntryCid
    pure $ MachineEntryIndex newEntryCid

-- | Get a sequence of expressions from a machine, starting from an entry
-- index. Pins the received expressions.
receiveIpfs :: Ipfs.IpnsId -> Maybe MachineEntryIndex -> IO (MachineEntryIndex, [Value])
receiveIpfs ipnsId maybeFrom = do
    let MachineEntryIndex fromCid = fromMaybe (MachineEntryIndex emptyMachineCid) maybeFrom
    Ipfs.NameResolveResponse cid <- Ipfs.nameResolve ipnsId
    blocks <- getBlocks cid fromCid
    pure $ (MachineEntryIndex cid, blocks)
  where
    getBlocks :: CID -> CID -> IO [Value]
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

-- | A node in the linked list of expressions of a machine.
data MachineEntry = MachineEntry
    { entryExpressions :: [Value]
    , entryPrevious    :: CID
    } deriving (Eq, Show, Read, Generic)

instance FromJSON MachineEntry where
    parseJSON = Aeson.withObject "MachineEntry" $ \o -> do
        expressionCodes <- o .: "expressions"
        let src = "[ipfs]"
        entryExpressions :: [Value] <-
            case traverse (parse src) expressionCodes of
                Left err  -> fail $ "failed to parse Radicle expression: " <> show err
                Right v -> pure v
        entryPrevious <- Ipfs.parseIpldLink =<< o .: "previous"
        pure MachineEntry {..}

instance ToJSON MachineEntry where
    toJSON MachineEntry{..} =
        let code = map renderCompactPretty entryExpressions
        in Aeson.object
            [ "expressions" .= code
            , "previous" .= Ipfs.ipldLink entryPrevious
            ]
