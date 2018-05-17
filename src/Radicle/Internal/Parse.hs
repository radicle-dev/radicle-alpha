{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Internal.Parse where

import           Control.Applicative (many, (<|>))
import           Control.Monad (void)
import           Data.Bifunctor (first)
import           Data.Char (isAlphaNum, isLetter)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Exts (IsString(..))
import           Text.Megaparsec (ParsecT, State(..), between, choice,
                                  defaultTabWidth, initialPos, manyTill, parse,
                                  runParser', sepBy, sepBy1, try, (<?>))
import           Text.Megaparsec.Char (char, satisfy, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Core

-- * The parser

type Parser m a = ParsecT Void Text m a

spaceConsumer :: Parser m ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";;"
    blockComment = L.skipBlockComment "#|" "|#" -- R6RS


symbol :: Text -> Parser m Text
symbol = L.symbol spaceConsumer

parensP :: Parser m a -> Parser m a
parensP = between (symbol "(" >> spaceConsumer) (spaceConsumer >> symbol ")")

primopP :: Parser m Value
primopP = Primop . Ident <$> (choice $ symbol . fromIdent <$> Map.keys primops)

stringLiteralP :: Parser m Value
stringLiteralP = String . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

boolLiteralP :: Parser m Value
boolLiteralP = Boolean <$> (char '#' >>
        (char 't' >> pure True) <|> (char 'f' >> pure False))

identP :: Parser m Ident
identP = do
    l <- satisfy isValidIdentFirst
    r <- many (satisfy isValidIdentRest)
    pure . Ident $ fromString (l:r)

atomP :: Parser m Value
atomP = Atom <$> identP

applyP :: Monad m => Parser m Value
applyP = do
    fn:args <- valueP `sepBy1` spaceConsumer
    pure $ fn $$ args

listP :: Monad m => Parser m Value
listP = List <$> (char '\'' >> parensP (valueP `sepBy` spaceConsumer))

sortedMapP :: Monad m => Parser m Value
sortedMapP = do
    void $ symbol "sorted-map"
    SortedMap . Map.fromList <$> pairP `sepBy` spaceConsumer
  where
    pairP = do
      (,) <$> identP <*> valueP

lambdaP :: Monad m => Parser m Value
lambdaP = do
    void $ symbol "lambda"
    vars <- parensP $ fmap atomToIdent <$> atomP `sepBy` spaceConsumer
    body <- valueP
    return $ Lambda vars body Nothing
  where
    atomToIdent (Atom i) = i
    atomToIdent _        = error "impossible"

valueP :: Monad m => Parser m Value
valueP = do
  spaceConsumer
  v <- choice
      [ stringLiteralP <?> "string"
      , boolLiteralP <?> "boolean"
      , try primopP <?> "primop"
      , atomP <?> "atom"
      , listP <?> "list"
      , parensP appLike <?> "application"
      ]
  spaceConsumer
  return v
  where
    appLike = choice
        [ lambdaP <?> "lambda"
        , sortedMapP <?> "sorted-map"
        , applyP <?> "application"
        ]

-- * Utilities

-- | Parse and evaluate a Text.
interpret :: String -> Text -> Either LangError Value
interpret sourceName expr = do
    parsed <- first ParseError $ parse valueP sourceName expr
    runLangM mempty $ eval parsed

-- | Parse a Text as a series of values.
-- 'sourceName' is used for error reporting.
--
-- Note that parsing continues even if one value fails to parse.
parseValues :: String -> Text -> [Either (Par.ParseError Char Void) Value]
parseValues sourceName srcCode = go $ initial
  where
    initial = State
        { stateInput = T.strip srcCode
        , statePos = initialPos sourceName :| []
        , stateTokensProcessed = 0
        , stateTabWidth = defaultTabWidth
        }
    go s = let (s', v) = runParser' valueP s
           in if T.null (stateInput s') then [v] else v:go s'

-- ** Valid identifiers
-- These are made top-level so construction of arbitrary instances that matches
-- parsing is easier

-- | A predicate which returns true if the character is valid as the first
-- character of an identifier.
isValidIdentFirst :: Char -> Bool
isValidIdentFirst x = isLetter x || x `elem` extendedChar

-- | A predicate which returns true if the character is valid as the second or
-- later character of an identifier.
isValidIdentRest :: Char -> Bool
isValidIdentRest x = isAlphaNum x || x `elem` extendedChar

extendedChar :: [Char]
extendedChar = ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<' , '=', '>'
  , '?', '@', '^', '_', '~']
