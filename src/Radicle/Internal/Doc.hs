module Radicle.Internal.Doc where

import           Protolude

import           Codec.Serialise (Serialise(..))
import           Data.Copointed (Copointed(..))
import           Data.Pointed (Pointed(..))

data Described a
  = NoDescription
  | Described Text a
  deriving (Show)

data Value
  = Fun Function
  | Other
  deriving (Show)

data Function = Function
  { parameters :: Described Params
  , output     :: Described Value
  } deriving (Show)

data Params
  = Fixed [Described Param]
  | Variable
  deriving (Show)

data Param = Param
  { name  :: Text
  , value :: Value
  } deriving (Show)

data Docd a = Docd (Maybe (Described Value)) a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Pointed Docd where
  point = Docd Nothing

instance Copointed Docd where
  copoint (Docd _ x) = x

type DocVal = Maybe (Described Value)

instance Serialise a => Serialise (Docd a) where
  encode (Docd _ x) = encode x
  decode = Docd Nothing <$> decode

instance Eq a => Eq (Docd a) where
  Docd _ x == Docd _ y = x == y

instance Ord a => Ord (Docd a) where
  compare (Docd _ x) (Docd _ y) = compare x y

noDocs :: [(a,c)] -> [(a, Maybe b, c)]
noDocs = fmap $ \(x,y) -> (x, Nothing, y)
