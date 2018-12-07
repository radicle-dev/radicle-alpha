-- | Defines and abstract interface 'StorageBackend' for Radicle
-- chains and 'buildStoragePrimFns' to add the storage backend as
-- primitive functions.
--
-- A storage backend defines two functions 'StorageSend' and
-- 'StorageReceive' explained below.
--
-- For usage examples see "Radicle.Internal.HttpStorage" and
-- "Radicle.Internal.TestCapabilities".
module Radicle.Internal.Storage
    ( StorageBackend(..)
    , buildStoragePrimFns
    ) where

import           Protolude hiding (TypeError)

import           GHC.Exts (fromList)
import           Servant.Client

import           Radicle.Internal.Core
import           Radicle.Internal.Identifier
import qualified Radicle.Internal.Number as Num
import qualified Radicle.Internal.PrimFns as PrimFns
import           Radicle.Internal.Type

-- | Definitions for @send@ and @receive@ functions of a storage
-- backend.
--
-- The first tuple item is the Radicle identifier the function will be
-- exposed as. The second tuple item is documentation.
data StorageBackend m = StorageBackend
    { storageSend    :: (Text, Text, StorageSend m)
    , storageReceive :: (Text, Text, StorageReceive m)
    }

-- | Send a list of expressions to a chain identified by the first
-- argument.
type StorageSend m = Text -> Seq Value -> m (Either ServantError ())

-- | Receive a list of expressions from a chain. The chain is identified by the first
-- argument. The second argument is the index from which to start.
-- Morally this is @'Data.List.drop' index chain@.
type StorageReceive m = Text -> Int -> m (Either Text [Value])

buildStoragePrimFns :: Monad m => StorageBackend m -> PrimFns m
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
                 Right _ -> pure $ Keyword $ unsafeToIdent "ok"
         (String _, v) -> throwErrorHere $ TypeError sendName 1 TVec v
         (v, _) -> throwErrorHere $ TypeError sendName 0 TString v
      )

    (receiveName, receiveDoc, receive) = storageReceive backend
    receivePrimop =
      ( receiveName
      , receiveDoc
      , PrimFns.twoArg receiveName $ \case
          (String id, Number q) -> do
              case Num.isInt q of
                  Left _ -> throwErrorHere . OtherError
                                     $ receiveName <> ": expecting int argument"
                  Right n -> do
                      res <- lift $ receive id n
                      case res of
                          Left err -> throwErrorHere . OtherError
                                    $ receiveName <> ": request failed: " <> show err
                          Right v' -> pure $ List v'
          (String _, v) -> throwErrorHere $ TypeError receiveName 1 TNumber v
          (v, _)        -> throwErrorHere $ TypeError receiveName 0 TString v
      )
