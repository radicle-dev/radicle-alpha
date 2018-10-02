module Radicle.Internal.Identifier where

import qualified Prelude
import           Protolude

import           Data.Char (isAlphaNum, isLetter)
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
extendedChar = ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<' , '=', '>'
  , '?', '@', '^', '_', '~']

-- | Expands characters into strings that are safe to use in keywords.
keywordChar :: Char -> Text
keywordChar x = case x of
      _ | isValidIdentRest x -> toS [x]
      '{'                    -> "_lbrace_"
      '}'                    -> "_rbrace_"
      '('                    -> "_lparen_"
      ')'                    -> "_rparen_"
      ' '                    -> "_space_"
      '\''                   -> "_prime_"
      ','                    -> "_comma_"
      '#'                    -> "_hash_"
      _                      -> "_" <> show (fromEnum x) <> "_"

-- | Makes a string safe to use in a keyword.
keywordWord :: Text -> Text
keywordWord "()" = "unit"
keywordWord x    = T.concat $ keywordChar <$> T.unpack x
