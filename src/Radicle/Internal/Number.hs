module Radicle.Internal.Number where

import           Protolude

import           Codec.Serialise (Serialise(..))
import           Data.Scientific
                 (Scientific, floatingOrInteger, toBoundedInteger)

import           Radicle.Internal.Orphans ()

data Number
  = Int Integer
  | Sci Scientific
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialise Number

pretty :: Number -> Text
pretty (Int i) = show i
pretty (Sci s) = show s

numFromIntegral :: Integral a => a -> Number
numFromIntegral = Int . fromIntegral

toSci :: Number -> Scientific
toSci (Sci s) = s
toSci (Int i) = fromIntegral i

fromSci :: Scientific -> Number
fromSci s = case sciInteger s of
  Just i -> Int i
  Nothing -> Sci s

isInteger :: Number -> Maybe Integer
isInteger (Int i) = pure i
isInteger (Sci s) = sciInteger s

isInt :: Number -> Maybe Int
isInt (Int i) =
    if inBounds then Just (fromIntegral i) else Nothing
  where
    inBounds = x <= i && i <= y
    x = fromIntegral (minBound :: Int)
    y = fromIntegral (maxBound :: Int)
isInt (Sci s) = toBoundedInteger s

sciInteger :: Scientific -> Maybe Integer
sciInteger s = case floatingOrInteger s of
  Left (_ :: Double) -> Nothing
  Right i -> Just i

op :: (forall a. Num a => a -> a -> a) -> Number -> Number -> Number
op o (Int x) (Int y) = Int (x `o` y)
op o (Sci x) (Sci y) = Sci (x `o` y)
op o (Int x) (Sci y) = Sci (fromIntegral x `o` y)
op o (Sci x) (Int y) = Sci (x `o` fromIntegral y)

comp :: (forall a. Ord a => a -> a -> Bool) -> Number -> Number -> Bool
comp c (Int x) (Int y) = x `c` y
comp c (Sci x) (Sci y) = x `c` y
comp c (Int x) (Sci y) = fromIntegral x `c` y
comp c (Sci x) (Int y) = x `c` fromIntegral y
