module Radicle.Internal.Number where

import           Protolude

import           Data.Scientific (Scientific, fromRationalRepetendUnlimited)

isInteger :: Rational -> Either Text Integer
isInteger (a :% b) | b == 1 = pure a
isInteger _        = Left "Not a whole number"

isInt :: Rational -> Either Text Int
isInt x = isInteger x >>= int
  where
    int i =
      if inBounds i then pure (fromIntegral i) else Left "Not a bounded whole number (int)"

    inBounds i =
         fromIntegral (minBound :: Int) <= i
      && i <= fromIntegral (maxBound :: Int)

isSci :: Rational -> Either Text Scientific
isSci r = case fromRationalRepetendUnlimited r of
  (s, Nothing) -> pure s
  _            -> Left "Does not have a non-repeating decimal expansion"
