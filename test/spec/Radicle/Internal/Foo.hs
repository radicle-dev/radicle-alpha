{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- | The purpose of the 'Foo' datatype is to check the instances of
-- 'FromRad' and 'ToRad' derived via generics.
module Radicle.Internal.Foo (Foo, Bar(..), Baz(..)) where

import           Protolude

import           Data.Scientific (Scientific)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Radicle

data Foo
  = FooA { a1 :: Text, a2 :: Scientific }
  | FooB { b1 :: Scientific, b2 :: Text}
  | FooC Text Scientific
  | FooD
  deriving (Eq, Show, Generic)

instance FromRad t Foo
instance ToRad t Foo
instance Arbitrary Foo where
  arbitrary = oneof
    [ FooA <$> arbitrary <*> arbitrary
    , FooB <$> arbitrary <*> arbitrary
    , FooC <$> arbitrary <*> arbitrary
    , pure FooD
    ]

data Bar =
  Bar { bar1 :: Text
      , bar2 :: Scientific
      } deriving (Eq, Show, Generic)

instance FromRad t Bar
instance ToRad t Bar
instance Arbitrary Bar where
  arbitrary = Bar <$> arbitrary <*> arbitrary

newtype Baz = Baz Text
  deriving (Eq, Show, Generic)

instance FromRad t Baz
instance ToRad t Baz
instance Arbitrary Baz where
  arbitrary = Baz <$> arbitrary
