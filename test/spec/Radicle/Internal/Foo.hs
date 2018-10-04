{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Radicle.Internal.Foo (Foo) where

import           Protolude

import           Data.Scientific (Scientific)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Radicle
import Radicle.Internal.Core

data Foo
  = FooA { a1 :: Text, a2 :: Scientific }
  | FooB { b1 :: Scientific, b2 :: Text}
  | FooC Text Scientific
  | FooD
  deriving (Eq, Show, Generic)

instance CPA t => FromRad t Foo
instance ToRad Foo
instance Arbitrary Foo where
  arbitrary = oneof
    [ FooA <$> arbitrary <*> arbitrary
    , FooB <$> arbitrary <*> arbitrary
    , FooC <$> arbitrary <*> arbitrary
    , pure FooD
    ]
