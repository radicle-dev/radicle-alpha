{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Lang.Internal.Orphans () where

import           Codec.Serialise (Serialise(..))
import           Data.Scientific
import           Prelude (fail)
import           Protolude
import           Text.Megaparsec
                 (ErrorFancy, ErrorItem, Pos, SourcePos, mkPos, unPos)

instance Serialise Scientific where
    encode = encode . (show :: Scientific -> Text)
    decode = decode >>= either fail pure . readEither

instance (Serialise a) => Serialise (ErrorItem a)
instance (Serialise a) => Serialise (ErrorFancy a)
instance Serialise SourcePos
instance Serialise Pos where
    encode = encode . unPos
    decode = mkPos <$> decode
