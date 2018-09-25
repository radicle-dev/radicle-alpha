{-# OPTIONS_GHC -fno-warn-orphans #-}

module Radicle.Internal.Crypto
  ( PublicKey
  , PrivateKey
  , Signature
  , hasher
  , verifySignature
  , signText
  , generateKeyPair
  ) where

import           Protolude

import           Codec.Serialise
import           Crypto.Hash.Algorithms
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.Types (CurveName(SEC_p256k1), getCurveByName)
import qualified Crypto.PubKey.ECC.Types as ECC
import           Crypto.Random.Types (MonadRandom)

hasher :: Blake2b_256
hasher = Blake2b_256

deriving instance Generic PublicKey
instance Serialise PublicKey
deriving instance Ord PublicKey

deriving instance Generic ECC.Curve
instance Serialise ECC.Curve
deriving instance Ord ECC.Curve

deriving instance Generic ECC.Point
instance Serialise ECC.Point
deriving instance Ord ECC.Point

deriving instance Generic ECC.CurveBinary
instance Serialise ECC.CurveBinary
deriving instance Ord ECC.CurveBinary

deriving instance Generic ECC.CurvePrime
instance Serialise ECC.CurvePrime
deriving instance Ord ECC.CurvePrime

deriving instance Generic ECC.CurveCommon
instance Serialise ECC.CurveCommon
deriving instance Ord ECC.CurveCommon

deriving instance Ord Signature

generateKeyPair :: MonadRandom m => m (PublicKey, PrivateKey)
generateKeyPair = generate (getCurveByName SEC_p256k1)

signText :: forall m. MonadRandom m => PrivateKey -> Text -> m Signature
signText key = signBytes . toS
  where
    signBytes :: ByteString -> m Signature
    signBytes = sign key hasher

-- | Verify a signed message with the public key.
verifySignature :: PublicKey -> Signature -> Text -> Bool
verifySignature key sig = verifyBytes . toS
  where
    verifyBytes :: ByteString -> Bool
    verifyBytes = verify hasher key sig
