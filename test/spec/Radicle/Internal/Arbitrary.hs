{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Arbitrary
    ( NoDoubleSpacesValue(..)
    ) where

import           Protolude

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Radicle
import qualified Radicle.Internal.Doc as Doc
import           Radicle.Internal.Identifier
                 (isValidIdentFirst, isValidIdentRest, Ident(..))
import           Radicle.Internal.PrimFns (purePrimFns)

instance Arbitrary r => Arbitrary (Env r) where
    arbitrary = Env <$> arbitrary

instance Arbitrary Value where
    arbitrary = sized (valueGenerator arbitrary)

newtype NoDoubleSpacesValue = NoDoubleSpacesValue Value
  deriving Show

instance Arbitrary NoDoubleSpacesValue where
    arbitrary = NoDoubleSpacesValue <$> sized (valueGenerator textNoDoubleSpaces)
      where textNoDoubleSpaces = arbitrary `suchThat` (not . T.isInfixOf "  ")

instance Arbitrary UntaggedValue where
    arbitrary = untag <$> (arbitrary :: Gen Value)

instance Arbitrary Ident where
    arbitrary = ((:) <$> firstL <*> rest) `suchThatMap` (mkIdent . toS)
      where
        allChars = take 100 ['!' .. maxBound]
        firstL = elements $ filter isValidIdentFirst allChars
        rest = sized $ \n -> do
          k <- choose (0, n)
          vectorOf k . elements $ filter isValidIdentRest allChars

instance Arbitrary a => Arbitrary (Bindings a) where
    arbitrary = do
        refs <- arbitrary
        env <- arbitrary
        prims <- arbitrary
        pure $ Bindings env mempty (Ident "toplevel") prims (IntMap.fromList $ zip [0..] refs)
            (length refs) mempty 0 mempty 0

instance Arbitrary a => Arbitrary (Doc.Docd a) where
    arbitrary = Doc.Docd Nothing <$> arbitrary

valueGenerator :: Gen Text -> Int -> Gen Value
valueGenerator textGen n | n == 0 = frequency $ first pred <$> freqs
                         | otherwise = frequency freqs
  where
    -- There's no literal syntax for dicts, only the 'dict' primop. If we
    -- generated them directly, we would generate something that can only
    -- be got at after an eval, and which doesn't really correspond to
    -- anything a user can write. So we don't generate dicts directly,
    -- instead requiring they go via the primop.
    freqs = [ (3, Atom <$> (arbitrary `suchThat` (\x -> not (isPrimop x || isNum x))))
            , (3, String <$> textGen)
            , (3, Boolean <$> arbitrary)
            , (3, Number <$> arbitrary)
            , (1, List <$> sizedList)
            , (6, PrimFn <$> elements (Map.keys $ getPrimFns prims))
            , (1, Lambda <$> lambdaArgs
                         <*> scale (`div` 4) arbitrary
                         <*> scale (`div` 4) arbitrary
                         <*> scale (`div` 4) arbitrary)
                         
            ]

    sizedList :: Arbitrary a => Gen [a]
    sizedList = sized $ \s -> do
        k <- choose (0, s)
        scale (`div` (k + 1)) $ vectorOf k arbitrary
    prims :: PrimFns Identity
    prims = purePrimFns
    isPrimop x = x `elem` Map.keys (getPrimFns prims)
    isNum x = isJust (readMaybe (toS $ fromIdent x) :: Maybe Scientific)
    lambdaArgs = oneof [ PosArgs <$> sizedList, VarArgs <$> arbitrary ]
