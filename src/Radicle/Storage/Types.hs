{-# LANGUAGE StrictData #-}

module Radicle.Storage.Types where

import           Protolude

import           Data.Aeson (FromJSON, ToJSON)

import           Radicle

-- | In-memory representation of the current state of a machine.
data Machine mid idx = Machine
    { machineId        :: mid
    , machineLastIndex :: Maybe idx
    , machineState     :: Bindings (PrimFns Identity)
    , machineMode      :: ReaderOrWriter
    }

-- | Class of types which can be rendered as a valid file name.
--
-- On *nix, this essentially means any character except the directory separator
-- \'/\' and NUL (\'\0\'). The 'FilePath' should also be < 251 characters.
--
class FilePathComponent a where
    toFilePathComponent :: a -> FilePath

-- | Constraint 'Backend'-specific machine identifiers must satisfy
type MachineId a =
    ( Eq       a
    , Show     a
    , Typeable a
    , Hashable a
    , ToJSON   a
    , FromJSON a
    , FilePathComponent a
    )

-- | Constraint 'Backend'-specific machine indices must satisfy
type MachineIndex a = (ToJSON a, FromJSON a)

-- | Indicates if the 'Machine' is writable by the local daemon.
data ReaderOrWriter = Reader | Writer
    deriving (Generic)

instance ToJSON ReaderOrWriter
instance FromJSON ReaderOrWriter

type Inputs  = Expressions
type Outputs = Expressions
type Nonce   = Text

data Expressions = Expressions
    { expressions :: [Value]
    , nonce       :: Maybe Nonce
    }

-- | A storage backend.
data Backend e mid idx m = Backend
    { backendPut :: mid -> Inputs -> m (Either e (PutResponse idx m))
    -- ^ Write input expressions to the 'Machine' identified by @mid@.
    --
    -- The 'Inputs' are guaranteed to be valid.
    --
    , backendGet :: mid -> idx -> m (Maybe [Value])
    -- ^ Get the input expressions of 'Machine' @mid@ at @idx@.
    --
    -- Return 'Nothing' if the given 'idx' could not be loaded. Throw an
    -- exception in 'm' if this is due to a transient error (such as I/O).
    --
    , backendFoldUpto
        :: forall a.
           mid
        -> idx
        -> (idx -> [Value] -> a -> a)
        -> a
        -> m (Either e a)
    -- ^ Fold a function over the history of 'Machine' @mid@, from initial up to
    -- and including index @idx@.
    --
    -- An error is returned if either @mid@ is unknown, @idx@ is unknown, or any
    -- of the inputs at an intermediate index can't be loaded.
    }

hoistBackend
    :: Functor m
    => (forall a. m a -> n a)
    -> Backend e mid idx m
    -> Backend e mid idx n
hoistBackend f b = Backend
    { backendPut = \mid inputs ->
        f $ second (hoistPutResponse f) <$> backendPut b mid inputs

    , backendGet = \mid idx ->
        f $ backendGet b mid idx

    , backendFoldUpto = \mid idx g a ->
        f $ backendFoldUpto b mid idx g a
    }

mapBackendError
    :: Functor m
    => (e -> e')
    -> Backend e  mid idx m
    -> Backend e' mid idx m
mapBackendError f b = b
    { backendPut       = \mid inputs  -> first f <$> backendPut b mid inputs
    , backendFoldUpto  = \mid idx g a ->
        first f <$> backendFoldUpto b mid idx g a
    }

-- | Response to 'backendPut'.
--
-- The new tip @idx@ of the 'Machine' after applying the inputs, the 'Nonce'
-- given with 'Inputs' (if any), and a continuation which signals the 'Backend'
-- that this new tip should now be published.
--
-- IFF the continuation is called, the 'Backend' MAY publish the new tip.
--
-- Calling the continuation MUST not block. Where publishing is an expensive
-- operation, the 'Backend' is responsible for implementing a threading model
-- allowing for asynchronous publishing.
--
data PutResponse idx m = PutResponse idx (Maybe Nonce) (m ())

hoistPutResponse
    :: (forall a. m a -> n a)
    -> PutResponse idx m
    -> PutResponse idx n
hoistPutResponse f (PutResponse idx nonce pub) =
    PutResponse idx nonce (f pub)
