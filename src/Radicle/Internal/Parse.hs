{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Internal.Parse where

import           Control.Applicative (many, (<|>))
import           Control.Monad (void, (>=>))
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader (Reader, ask, runReader)
import           Control.Monad.State (gets)
import           Data.Char (isAlphaNum, isLetter)
import           Data.Either (partitionEithers)
import           Data.Functor.Foldable (Fix(..))
import           Data.List.NonEmpty (NonEmpty((:|)), fromList)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Exts (IsString(..))
import           Text.Megaparsec (ParsecT, State(..), between, choice,
                                  defaultTabWidth, eof, initialPos, manyTill,
                                  runParserT, runParserT', sepBy, some, try,
                                  (<?>))
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char (char, satisfy, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Core

-- * The parser

type Parser a = ParsecT Void Text (Reader [Ident]) a
type VParser = Parser (Value (Fix Value))

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";;"
    blockComment = L.skipBlockComment "#|" "|#" -- R6RS


symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

parensP :: Parser a -> Parser a
parensP = between (symbol "(" >> spaceConsumer) (spaceConsumer >> symbol ")")

stringLiteralP :: VParser
stringLiteralP = lexeme $
    String . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

boolLiteralP :: VParser
boolLiteralP = lexeme $ Boolean <$> (char '#' >>
        (char 't' >> pure True) <|> (char 'f' >> pure False))

numLiteralP :: VParser
numLiteralP = Number <$> signed L.scientific
  where
    -- We don't allow spaces between the sign and digits so that we can remain
    -- consistent with the general Scheme of things.
    signed p = M.option id ((id <$ char '+') <|> (negate <$ char '-')) <*> p

identP :: Parser Ident
identP = lexeme $ do
    l <- satisfy isValidIdentFirst
    r <- many (satisfy isValidIdentRest)
    pure . Ident $ fromString (l:r)

atomOrPrimP :: VParser
atomOrPrimP = do
    i <- identP
    prims <- ask
    pure $ if i `elem` prims then Primop i else Atom i

applyP :: VParser
applyP = List <$> valueP `sepBy` spaceConsumer

quoteP :: VParser
quoteP = List . ((Primop $ toIdent "quote") :) . pure <$> (char '\'' >> valueP)

dictP :: VParser
dictP = do
    void $ symbol "dict"
    Dict . Map.fromList <$> pairP `sepBy` spaceConsumer
  where
    pairP = (,) <$> identP <*> valueP

lambdaP :: VParser
lambdaP = do
    void $ symbol "lambda"
    vars <- parensP $ identP `sepBy` spaceConsumer
    body <- fromList <$> some valueP
    pure $ Lambda vars body Nothing

refP :: VParser
refP = do
    void $ symbol "ref"
    Ref . Fix <$> valueP

valueP :: VParser
valueP = do
  v <- choice
      [ stringLiteralP <?> "string"
      , boolLiteralP <?> "boolean"
      , try numLiteralP <?> "number"
      , atomOrPrimP <?> "identifier"
      , quoteP <?> "quote"
      , parensP appLike <?> "application"
      ]
  spaceConsumer
  pure v
  where
    appLike = choice
        [ lambdaP <?> "lambda"
        , dictP <?> "dict"
        , refP <?> "ref"
        , applyP <?> "application"
        ]

-- * Utilities

-- | Parse and evaluate a Text. Replaces refs with a number representing them.
--
-- Examples:
--
-- >>> import Control.Monad.Identity
-- >>> runIdentity $ interpret "test" "((lambda (x) x) #t)" pureEnv
-- Right (Boolean True)
--
-- >>> import Control.Monad.Identity
-- >>> runIdentity $ interpret "test" "(#t #f)" pureEnv
-- Left (TypeError "Trying to apply a non-function")
interpret
    :: Monad m
    => String
    -> Text
    -> Bindings m
    -> m (Either (LangError (Value Reference)) (Value Reference))
interpret sourceName expr bnds = do
    let primopNames = Map.keys (bindingsPrimops bnds)
        parsed = runReader (runParserT (valueP <* eof) sourceName expr) primopNames
    case parsed of
        Left e  -> pure . Left $ ParseError e
        Right v -> runLang bnds (makeRefs v >>= eval)

-- | Parse and evaluate a Text as multiple expressions.
--
-- Examples:
--
-- >>> runLang pureEnv $ interpretMany "test" "(define id (lambda (x) x))\n(id #t)"
-- Right (Boolean True)
interpretMany :: Monad m => String -> Text -> Lang m (Value Reference)
interpretMany sourceName src = do
    primopNames <- gets $ Map.keys . bindingsPrimops
    let parsed = parseValues sourceName src primopNames
    case partitionEithers parsed of
        ([], vs) -> last <$> mapM (makeRefs >=> eval) vs
        (e:_, _) -> throwError $ ParseError e

-- | Parse a Text as a series of values.
-- 'sourceName' is used for error reporting. 'prims' are the primop names.
--
-- Note that parsing continues even if one value fails to parse.
parseValues :: String -> Text -> [Ident] -> [Either (Par.ParseError Char Void) (Value (Fix Value))]
parseValues sourceName srcCode prims = go $ initial
  where
    initial = State
        { stateInput = T.strip srcCode
        , statePos = initialPos sourceName :| []
        , stateTokensProcessed = 0
        , stateTabWidth = defaultTabWidth
        }
    go s = let (s', v) = runReader (runParserT' valueP s) prims
           in if T.null (stateInput s') then [v] else v:go s'

-- | Parse a value, using the String as source name, and the identifier list as
-- the primops.
--
-- Examples:
--
-- >>> parse "test" "#t" [] :: Either String (Value (Fix Value))
-- Right (Boolean True)
--
-- >>> parse "test" "hi" [toIdent "hi"] :: Either String (Value (Fix Value))
-- Right (Primop (Ident {fromIdent = "hi"}))
--
-- >>> parse "test" "hi" [] :: Either String (Value (Fix Value))
-- Right (Atom (Ident {fromIdent = "hi"}))
parse :: MonadError String m => String -> Text -> [Ident] -> m (Value (Fix Value))
parse file src ids = do
  let res = runReader (M.runParserT (valueP <* eof) file src) ids
  case res of
    Left err -> throwError $ M.parseErrorPretty' src err
    Right v  -> pure v

-- | Like 'parse', but uses "(test)" as the source name and the default set of
-- primops.
parseTest :: MonadError String m => Text -> m (Value (Fix Value))
parseTest t = parse "(test)" t (Map.keys $ bindingsPrimops e)
  where
    e :: Bindings (Lang Identity)
    e = pureEnv


-- ** Valid identifiers
-- These are made top-level so construction of arbitrary instances that matches
-- parsing is easier. Note that additionally an identifier must not be a valid
-- number (in parsing numbers are parsed first).

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
