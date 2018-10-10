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

hash :: ByteString -> ByteString
hash = show . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.Blake2b_256)

hashRad :: (Copointed t, Annotation t) => Annotated t ValueF -> ByteString
hashRad = hash . toS . renderCompactPretty

