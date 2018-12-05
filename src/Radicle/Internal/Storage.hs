-- | Defines and abstract interface 'StorageBackend' for Radicle
-- chains and 'buildStoragePrimFns' to add the storage backend as
-- primitive functions.
--
-- A storage backend defines two functions 'StorageSend' and
-- 'StorageReceive' explained below.
--
-- Storage backends use /indices/ to identify entries in a chain. These
-- indices can be used like cursors in a database.
--
-- For usage examples see "Radicle.Internal.HttpStorage" and
-- "Radicle.Internal.TestCapabilities".
module Radicle.Internal.Storage
    ( StorageBackend(..)
    , buildStoragePrimFns
    ) where

import           Protolude hiding (TypeError)

import           GHC.Exts (fromList)

import           Radicle.Internal.Annotation (WithPos)
import           Radicle.Internal.Core
import qualified Radicle.Internal.PrimFns as PrimFns
import           Radicle.Internal.Type

-- | Definitions for @send@ and @receive@ functions of a storage
-- backend.
--
-- The first tuple item is the Radicle identifier the function will be
-- exposed as. The second tuple item is documentation.
data StorageBackend i m = StorageBackend
    { storageSend    :: (Text, Text, StorageSend i m)
    , storageReceive :: (Text, Text, StorageReceive i m)
    }

-- | Send a list of expressions to a chain identified by the first
-- argument. Returns an error or the index of the expression that was
-- sent.
type StorageSend i m = Text -> Seq Value -> m (Either Text i)

-- | Receive all expressions following the given expression and a new
-- index for further queries.
--
-- If second arugment is @'Just' i@ then we return all expressions that
-- follow the expression index by @i@ and not including that
-- expression.
--
-- If the second argument is 'Nothing' we return all expressions.
--
-- The first item in the tuple returned is the index of the last entry
-- in the list of expressions returned.
type StorageReceive i m = Text -> Maybe i -> m (Either Text (i, [Value]))

buildStoragePrimFns :: forall i m. (Monad m, FromRad WithPos i, ToRad WithPos i) => StorageBackend i m -> PrimFns m
buildStoragePrimFns backend =
    fromList . PrimFns.allDocs $ [sendPrimop, receivePrimop]
  where
    (sendName, sendDoc, send) = storageSend backend
    sendPrimop =
      ( sendName
      , sendDoc
      , PrimFns.twoArg sendName $ \case
         (String id, Vec v) -> do
             res <- lift $ send id v
             case res of
                 Left e  -> throwErrorHere (SendError e)
                 Right r -> pure $ toRad r
         (String _, v) -> throwErrorHere $ TypeError sendName 1 TVec v
         (v, _) -> throwErrorHere $ TypeError sendName 0 TString v
      )

    (receiveName, receiveDoc, receive) = storageReceive backend
    receivePrimop =
      ( receiveName
      , receiveDoc
      , PrimFns.twoArg receiveName $ \case
          (String id, v) -> do
              index <- case fromRad v of
                  Right Nothing -> pure Nothing
                  Right (Just n) -> pure (Just n)
                  Left e ->  throwReceiveError $ "failed to parse second argument: " <> e
              res <- lift $ receive id index
              case res of
                  Left err -> throwReceiveError $ "request failed: " <> err
                  Right (newIndex, values) -> pure $ Vec $ fromList [toRad newIndex, Vec $ fromList values]
          (v, _)        -> throwErrorHere $ TypeError receiveName 0 TString v
      )

    throwReceiveError :: Text -> Lang m a
    throwReceiveError msg = throwErrorHere $ OtherError $ receiveName <> ": " <> msg
