{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Orphans () where

import Protolude hiding (hash)
import Prelude (fail)
import Codec.Serialise (Serialise(..))
import qualified Hash as AADT
import Crypto.Hash (hash)
import Data.Scientific
import qualified Data.Map as Map

instance Serialise Scientific where
    encode = encode . (show :: Scientific -> Text)
    decode = decode >>= either fail pure . readEither

instance (AADT.Hashable k, AADT.Hashable v) => AADT.Hashable (Map k v) where
    toHash = AADT.toHash . foldMap (\(k,v) -> AADT.toBS k <> AADT.toBS v) . Map.toList

instance (AADT.Hashable v) => AADT.Hashable [v] where
    toHash = AADT.toHash . foldMap AADT.toBS

instance (AADT.Hashable v) => AADT.Hashable (NonEmpty v) where
    toHash = AADT.toHash . foldMap AADT.toBS

instance (AADT.Hashable v) => AADT.Hashable (Maybe v) where

instance AADT.Hashable Scientific where
    toHash = AADT.Hash . hash . (show :: Scientific -> ByteString)
