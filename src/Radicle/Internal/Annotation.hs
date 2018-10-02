{-# LANGUAGE UndecidableInstances #-}

module Radicle.Internal.Annotation where

import           Codec.Serialise (Serialise)
import           Data.Copointed (Copointed(..))
import           Data.Text (pack)
import           Protolude
import qualified Text.Megaparsec.Pos as Par

import           Radicle.Internal.Orphans ()

newtype Annotated t f = Annotated (t (f (Annotated t f)))
    deriving (Generic)

-- Really should depend on Eq1 etc. but that is so much boilerplate.
-- Instead we use some UndecidableInstances.
deriving instance (Eq (t (f (Annotated t f)))) => Eq (Annotated t f)
deriving instance (Ord (t (f (Annotated t f)))) => Ord (Annotated t f)
deriving instance (Read (t (f (Annotated t f)))) => Read (Annotated t f)
deriving instance (Show (t (f (Annotated t f)))) => Show (Annotated t f)
deriving instance (Serialise (t (f (Annotated t f)))) => Serialise (Annotated t f)

class Annotation t where
    -- We use the call stack to create source location annotations for interpreter-created objects
    toAnnotation :: HasCallStack => a -> t a

instance Annotation Identity where
    toAnnotation = Identity

untag :: (Functor f, Copointed t) => Annotated t f -> Annotated Identity f
untag (Annotated t) = Annotated . Identity $ untag <$> copoint t

tagDefault :: (Functor f, Annotation t) => Annotated Identity f -> Annotated t f
tagDefault (Annotated (Identity t)) = Annotated (toAnnotation (tagDefault <$> t))

match :: (Copointed t) => Annotated t f -> f (Annotated t f)
match (Annotated t) = copoint t

annotate :: (HasCallStack, Annotation t) => f (Annotated t f) -> Annotated t f
annotate = withFrozenCallStack (Annotated . toAnnotation)


data SrcPos = SrcPos Par.SourcePos
            -- | If not associated with a source position, record where
            -- in the interpreter this node was created (if only to know
            -- what we need to improve).
            --
            -- We use Text instead of CallStack because CallStack has no
            -- Read instance, and it's not worth it to make a parser.
            | InternalPos Text
    deriving (Eq, Ord, Read, Show, Generic)

instance Serialise SrcPos

thisPos :: HasCallStack => SrcPos
thisPos = InternalPos (pack (prettyCallStack callStack))

data WithPos a = WithPos SrcPos a
    deriving (Read, Show, Generic, Functor, Foldable, Traversable)

instance Serialise a => Serialise (WithPos a)

-- Ignore source locations when comparing.
instance Eq a => Eq (WithPos a) where
    WithPos _ x == WithPos _ y = x == y

instance (Ord a) => Ord (WithPos a) where
    compare (WithPos _ x) (WithPos _ y) = compare x y

instance Copointed WithPos where
    copoint (WithPos _ x) = x

instance Annotation WithPos where
    toAnnotation = WithPos thisPos

