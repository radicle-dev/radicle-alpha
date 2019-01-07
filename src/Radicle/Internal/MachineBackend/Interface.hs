-- | This module defines an abstract 'MachineBackend' interface to
-- interact with Radicle State Machines. We provide the
-- 'buildMachinePrimFns' to add an implementation of 'MachineBackend'
-- to the primitive functions of the interpreter
--
-- A storage backend defines two functions 'MachineUpdate' and
-- 'MachineGetLog' explained below.
--
-- Machine backends use /indices/ to identify entries in a chain. These
-- indices can be used like cursors in a database.
--
-- For usage examples see "Radicle.Internal.HttpStorage" and
-- "Radicle.Internal.TestCapabilities".
module Radicle.Internal.MachineBackend.Interface
    ( MachineBackend(..)
    , buildMachineBackendPrimFns
    ) where

import           Protolude hiding (TypeError)

import           GHC.Exts (fromList)

import           Radicle.Internal.Annotation (WithPos)
import           Radicle.Internal.Core
import           Radicle.Internal.Identifier (unsafeToIdent)
import qualified Radicle.Internal.PrimFns as PrimFns
import           Radicle.Internal.Type

-- | Definitions for @send@ and @receive@ functions of a storage
-- backend.
--
-- The first tuple item is the Radicle identifier the function will be
-- exposed as. The second tuple item is documentation.
data MachineBackend i m = MachineBackend
    { machineType   :: Text
    , machineUpdate :: MachineUpdate i m
    , machineGetLog :: MachineGetLog i m
    }

-- | Send a list of expressions to a chain identified by the first
-- argument. Returns an error or the index of the expression that was
-- sent.
type MachineUpdate i m = Text -> Seq Value -> m (Either Text i)

-- | Get all inputs to a machine following the given input index and
-- return a new index for further queries.
--
-- If second arugment is @'Just' i@ then we return all input expresions
-- that follow the input indexed by @i@ and not including that
-- input.
--
-- If the second argument is 'Nothing' we return all expressions.
--
-- The first item in the tuple returned is the index of the last entry
-- in the list of inputs returned.
type MachineGetLog i m = Text -> Maybe i -> m (Either Text (i, [Value]))

buildMachineBackendPrimFns
    :: forall i m. (Monad m, FromRad WithPos i, ToRad WithPos i)
    => MachineBackend i m -> PrimFns m
buildMachineBackendPrimFns backend =
    fromList [ (unsafeToIdent updateName, Nothing, updatePrimFn)
             , (unsafeToIdent getLogName, Nothing, getLogPrimFn)]
  where
    updateName = "machine/" <> machineType backend <> "/update!"
    updatePrimFn =
        PrimFns.twoArg updateName $ \case
         (String id, Vec v) -> do
             res <- lift $ machineUpdate backend id v
             case res of
                 Left e  -> throwErrorHere (SendError e)
                 Right r -> pure $ toRad r
         (String _, v) -> throwErrorHere $ TypeError updateName 1 TVec v
         (v, _) -> throwErrorHere $ TypeError updateName 0 TString v

    getLogName = "machine/" <> machineType backend <> "/get-log!"
    getLogPrimFn =
        PrimFns.twoArg updateName $ \case
          (String id, v) -> do
              index <- case fromRad v of
                  Right Nothing -> pure Nothing
                  Right (Just n) -> pure (Just n)
                  Left e ->  throwReceiveError $ "failed to parse second argument: " <> e
              res <- lift $ machineGetLog backend id index
              case res of
                  Left err -> throwReceiveError $ "request failed: " <> err
                  Right (newIndex, values) -> pure $ Vec $ fromList [toRad newIndex, Vec $ fromList values]
          (v, _)        -> throwErrorHere $ TypeError getLogName 0 TString v

    throwReceiveError :: Text -> Lang m a
    throwReceiveError msg = throwErrorHere $ OtherError $ getLogName <> ": " <> msg
