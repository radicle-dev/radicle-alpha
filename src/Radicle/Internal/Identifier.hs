{-# LANGUAGE PatternSynonyms #-}

module Radicle.Internal.Identifier where

import qualified Prelude
import           Protolude

import           Codec.Serialise (Serialise)
import           Data.Char (isAlphaNum, isLetter, isUpper, toLower)
import qualified Data.Map as Map
import qualified Data.Text as T

-- These are made top-level so construction of arbitrary instances that matches
-- parsing is easier. Note that additionally an identifier must not be a valid
-- number (in parsing numbers are parsed first).

-- | A predicate which returns true if the character is valid as the first
-- character of an identifier.
isValidIdentFirst :: Char -> Bool
isValidIdentFirst x = x /= ':' && (isLetter x || x `elem` extendedChar)

-- | A predicate which returns true if the character is valid as the second or
-- later character of an identifier.
isValidIdentRest :: Char -> Bool
isValidIdentRest x = isAlphaNum x || x `elem` extendedChar

extendedChar :: Prelude.String
extendedChar = ['!', '$', '%', '&', '*', '+', '-', '.', ':', '<' , '=', '>'
  , '?', '@', '^', '_', '~']

replacements :: Map Char Prelude.String
replacements = Map.fromList
  [ ('{', "_lbrace_")
  , ('}', "_rbrace_")
  , ('[', "_lbrack_")
  , (']', "_rbrack_")
  , ('(', "_lparen_")
  , (')', "_rparen_")
  , ('|', "_pipe_")
  , (' ', "_space_")
  , ('\'', "_prime_")
  , (',', "_comma_")
  , ('#', "_hash_")
  ]

-- | Converts a Haskell constructor name into a valid radicle identifier.
kebabCons :: Prelude.String -> Text
kebabCons = T.intercalate "-" . fmap (toS . lowerFirst) . go .keywordWord
  where
    go "" = []
    go xs@(x:_) | isLower x = lowers : go rest
      where
        (lowers, rest) = Prelude.span isLower xs
        isLower = not . isUpper
    go (x:xs) = consCase x rest
      where
        rest = go xs
        consCase c []     = [[c]]
        consCase c (p:ps) = (c:p):ps

    lowerFirst ""     = ""
    lowerFirst (c:cs) = toLower c : cs

    -- Expands characters into strings that are safe to use in keywords.
    keywordChar x | isValidIdentRest x = [x]
    keywordChar x =
      case Map.lookup x replacements of
        Just r  -> r
        Nothing -> "_" <> show (fromEnum x) <> "_"

    -- Makes a string safe to use in a keyword.
    keywordWord = concat . fmap keywordChar

data Unnamespaced
  = Naked Text
  | Qualified Text Text
  deriving (Eq, Ord, Read, Show, Generic)

instance Serialise Unnamespaced

showUnnamespaced :: Unnamespaced -> Text
showUnnamespaced = \case
  Naked x -> x
  Qualified q x -> q <> "/" <> x

-- | An symbol in the language.
--
-- Not all `Text`s are valid identifiers, so use 'Ident' at your own risk.
-- `mkIdent` is the safe version.
data Ident
  = Unnamespaced Unnamespaced
  | Namespaced Text Unnamespaced
  deriving (Eq, Show, Read, Ord, Generic)

instance Serialise Ident

showIdent :: Ident -> Text
showIdent = \case
  Unnamespaced x -> showUnnamespaced x
  Namespaced n x -> n <> "//" <> showUnnamespaced x

-- -- | Convert a text to an identifier.
-- --
-- -- Unsafe! Only use this if you know the string at compile-time and know it's a
-- -- valid identifier. Otherwise, use 'mkIdent'.
-- unsafeToIdent :: Text -> Ident
-- unsafeToIdent = Ident
