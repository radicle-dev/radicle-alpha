{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Orphans () where

import Protolude
import Prelude (fail)
import Codec.Serialise (Serialise(..))
import Data.Scientific

instance Serialise Scientific where
    encode = encode . (show :: Scientific -> Text)
    decode = decode >>= either fail pure . readEither
