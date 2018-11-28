module Radicle.Internal.Annotation where

import           Codec.Serialise (Serialise)
import           Protolude
import qualified Text.Megaparsec.Pos as Par

import           Radicle.Internal.Orphans ()

data SrcPos
    = SrcPos Par.SourcePos
    deriving (Eq, Ord, Read, Show, Generic)

instance Serialise SrcPos
