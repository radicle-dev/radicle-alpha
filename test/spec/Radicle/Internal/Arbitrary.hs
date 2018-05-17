{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Arbitrary where

import           Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Radicle
import           Radicle.Internal.Parse (isValidIdentFirst, isValidIdentRest)

instance Arbitrary Env where
    arbitrary = Env <$> arbitrary

instance Arbitrary Value where
    arbitrary = sized go
      where
        freqs = [ (3, Atom <$> arbitrary)
                , (3, String <$> arbitrary)
                , (3, Boolean <$> arbitrary)
                , (1, List <$> sizedList)
                , (3, Primop <$> elements (Map.keys primops))
                , (1, Apply <$> scale (`div` 2) arbitrary <*> sizedList)
                , (3, SortedMap . Map.fromList <$> sizedList)
                , (1, Lambda <$> sizedList
                             <*> scale (`div` 3) arbitrary
                             <*> scale (`div` 3) arbitrary)
                ]
        go n | n == 0 = frequency $ first pred <$> freqs
             | otherwise = frequency freqs
        sizedList :: Arbitrary a => Gen [a]
        sizedList = sized $ \n -> do
            k <- choose (0, n)
            scale (`div` (k + 1)) $ vectorOf k arbitrary


instance Arbitrary Ident where
    arbitrary = ((:) <$> firstL <*> rest) `suchThatMap` (makeIdent . T.pack)
      where
        allChars = take 100 ['!' .. maxBound]
        firstL = elements $ filter isValidIdentFirst allChars
        rest = sized $ \n -> do
          k <- choose (0, n)
          vectorOf k . elements $ filter isValidIdentRest allChars
