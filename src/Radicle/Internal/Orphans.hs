{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Orphans () where

import           Codec.Serialise (Serialise(..))
import           Data.Scientific
import           Prelude (fail)
import           Protolude

instance Serialise Scientific where
    encode = encode . (show :: Scientific -> Text)
    decode = decode >>= either fail pure . readEither
