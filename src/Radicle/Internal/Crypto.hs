{-# OPTIONS_GHC -fno-warn-orphans #-}

module Radicle.Internal.Crypto
  ( PublicKey
  , PrivateKey
  , Signature
  , hasher
  , defaultCurve
  , verifySignature
  , signText
  , generateKeyPair
  , MonadRandom(..)
  ) where

import           Protolude

import           Crypto.Hash.Algorithms
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.Types
import           Crypto.Random
import           Crypto.Random.Types (MonadRandom)

import           Radicle.Internal.Core

deriving instance Generic PublicKey
instance FromRad PublicKey
instance ToRad PublicKey

deriving instance Generic PrivateKey
instance FromRad PrivateKey
instance ToRad PrivateKey

deriving instance Generic Curve
instance FromRad Curve
instance ToRad Curve

deriving instance Generic Point
instance FromRad Point
instance ToRad Point

deriving instance Generic CurveBinary
instance FromRad CurveBinary
instance ToRad CurveBinary

deriving instance Generic CurvePrime
instance FromRad CurvePrime
instance ToRad CurvePrime

deriving instance Generic CurveCommon
instance FromRad CurveCommon
instance ToRad CurveCommon

deriving instance Generic Signature
instance FromRad Signature
instance ToRad Signature

hasher :: Blake2b_256
hasher = Blake2b_256

generateKeyPair :: MonadRandom m => Curve -> m (PublicKey, PrivateKey)
generateKeyPair = generate

defaultCurve :: Curve
defaultCurve = getCurveByName SEC_p256k1

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
