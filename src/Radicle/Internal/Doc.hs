{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Radicle.Internal.Doc where

import           Protolude hiding (Any)

import           Codec.Serialise (Serialise(..))
import           Data.Copointed (Copointed(..))

data Docd a = Docd (Maybe Text) a
  deriving (Show, Read, Functor, Foldable, Traversable, Generic)

instance Serialise a => Serialise (Docd a)

instance Copointed Docd where
  copoint (Docd _ x) = x

doc :: Docd a -> Maybe Text
doc (Docd d _) = d

instance Eq a => Eq (Docd a) where
  Docd _ x == Docd _ y = x == y

instance Ord a => Ord (Docd a) where
  compare (Docd _ x) (Docd _ y) = compare x y

noDocs :: [(a, c)] -> [(a, Maybe b, c)]
noDocs = fmap $ \(x,y) -> (x, Nothing, y)

