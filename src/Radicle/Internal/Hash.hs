module Radicle.Internal.Hash
  ( hashRad
  , hash
  ) where

import           Protolude hiding (hash)

import qualified Crypto.Hash as Crypto
import           Data.Copointed (Copointed(..))

import           Radicle.Internal.Annotation
import           Radicle.Internal.Core
import           Radicle.Internal.Pretty

type BS = ByteString

-- = Hashing of radicle values

blake2b_256 :: ByteString -> Crypto.Digest Crypto.Blake2b_256
blake2b_256 = Crypto.hash

hash :: BS -> BS
hash = show . blake2b_256

hashRad :: (Copointed t, Annotation t) => Annotated t ValueF -> BS
hashRad = hash . toS . renderCompactPretty

