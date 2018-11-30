{-# LANGUAGE PackageImports #-}
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
import           "cryptonite" Crypto.Random
import           Crypto.Random.Types (MonadRandom)

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core

deriving instance Generic PublicKey
instance FromRad Ann.WithPos PublicKey
instance ToRad Ann.WithPos PublicKey

deriving instance Generic PrivateKey
instance FromRad Ann.WithPos PrivateKey
instance ToRad Ann.WithPos PrivateKey

deriving instance Generic Curve
instance FromRad Ann.WithPos Curve
instance ToRad Ann.WithPos Curve

deriving instance Generic Point
instance FromRad Ann.WithPos Point
instance ToRad Ann.WithPos Point

deriving instance Generic CurveBinary
instance FromRad Ann.WithPos CurveBinary
instance ToRad Ann.WithPos CurveBinary

deriving instance Generic CurvePrime
instance FromRad Ann.WithPos CurvePrime
instance ToRad Ann.WithPos CurvePrime

deriving instance Generic CurveCommon
instance FromRad Ann.WithPos CurveCommon
instance ToRad Ann.WithPos CurveCommon

deriving instance Generic Signature
instance FromRad Ann.WithPos Signature
instance ToRad Ann.WithPos Signature

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
