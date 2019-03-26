module Radicle.Internal.Number where

import           Protolude

import qualified Data.Scientific as Scientific

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

toBoundedInteger :: (Integral i, Bounded i) => Rational -> Maybe i
toBoundedInteger r =
    case isSci r of
        Left _  -> Nothing
        Right s -> Scientific.toBoundedInteger s

isSci :: Rational -> Either Text Scientific.Scientific
isSci r = case Scientific.fromRationalRepetendUnlimited r of
  (s, Nothing) -> pure s
  _            -> Left $ "The rational number " <> show r <> " does not have a non-repeating decimal expansion"
