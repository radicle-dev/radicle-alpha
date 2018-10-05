module Radicle.Internal.Hash (hashRad) where

import           Protolude hiding (hash)

import           Crypto.Hash
import           Data.Copointed (Copointed(..))

import           Radicle.Internal.Annotation
import           Radicle.Internal.Core
import           Radicle.Internal.Pretty

blake2b_256 :: ByteString -> Digest Blake2b_256
blake2b_256 = hash

hashRad :: (Copointed t, Annotation t) => Annotated t ValueF -> Text
hashRad = show . blake2b_256 . toS . renderCompactPretty
