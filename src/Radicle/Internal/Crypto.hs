{-# OPTIONS_GHC -fno-warn-orphans #-}

module Radicle.Internal.Crypto
  ( PublicKey
  , PrivateKey
  , Signature
  , hasher
  , verifySignature
  , signText
  , generateKeyPair
  , radToPubKey
  , radToPrivKey
  , pubKeyToRad
  , privKeyToRad
  , radToSignature
  , signatureToRad
  , MonadRandom(..)
  ) where

import           Protolude

import           Crypto.Hash.Algorithms
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.PubKey.ECC.Generate (generate)
import           Crypto.PubKey.ECC.Types
import           Crypto.Random
import           Crypto.Random.Types (MonadRandom)
import qualified Data.Map.Strict as Map

import           Radicle.Internal.Core

hasher :: Blake2b_256
hasher = Blake2b_256

curve :: Curve
curve = getCurveByName SEC_p256k1

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

-- To/From radicle

-- TODO: use ToRadicle/FromRadicle typeclasses when they are available.

-- Turns radicle values in public keys for curve SEC_p256
radToPubKey :: Value -> Maybe PublicKey
radToPubKey = \case
  List [Keyword (Ident "PointO")] -> pure $
    PublicKey curve PointO
  List [Keyword (Ident "Point"), x, y] -> do
    i <- isInt x
    j <- isInt y
    pure $ PublicKey curve (Point i j)
  _ -> Nothing

radToPrivKey :: Value -> Maybe PrivateKey
radToPrivKey v = PrivateKey curve <$> isInt v

pubKeyToRad :: PublicKey -> Maybe Value
pubKeyToRad (PublicKey c p) =
  if c == curve
  then case p of
    PointO -> pure $ List [Keyword (Ident "PointO")]
    Point x y -> pure . List $
      [ Keyword (Ident "Point")
      , Number (fromIntegral x)
      , Number (fromIntegral y)
      ]
  else Nothing

privKeyToRad :: PrivateKey -> Maybe Value
privKeyToRad (PrivateKey c d) =
  if c == curve
  then pure (Number (fromIntegral d))
  else Nothing

radToSignature :: Value -> Maybe Signature
radToSignature = \case
  Dict d -> do
    r <- kwLookup "sign_r" d
    s <- kwLookup "sign_s" d
    Signature <$> isInt r <*> isInt s
  _ -> Nothing

signatureToRad :: Signature -> Value
signatureToRad (Signature r s) = Dict . Map.fromList $
  [ (Keyword (Ident "sign_r"), Number (fromIntegral r))
  , (Keyword (Ident "sign_s"), Number (fromIntegral s))
  ]
