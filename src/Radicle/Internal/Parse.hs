{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Internal.Parse where

import           Control.Applicative (many, (<|>))
import           Control.Monad (void)
import           Control.Monad.Except
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader (Reader, ask, runReader)
import           Control.Monad.State (gets)
import           Data.Char (isAlphaNum, isLetter)
import           Data.Either (partitionEithers)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Exts (IsString(..))
import           Text.Megaparsec (ParsecT, State(..), between, choice,
                                  defaultTabWidth, initialPos, manyTill,
                                  runParserT, runParserT', sepBy, sepBy1,
                                  (<?>), eof)
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char (char, satisfy, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Core

-- * The parser

type Parser a = ParsecT Void Text (Reader [Ident]) a

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

stringLiteralP :: Parser Value
stringLiteralP = lexeme $
    String . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

boolLiteralP :: Parser Value
boolLiteralP = lexeme $ Boolean <$> (char '#' >>
        (char 't' >> pure True) <|> (char 'f' >> pure False))

identP :: Parser Ident
identP = lexeme $ do
    l <- satisfy isValidIdentFirst
    r <- many (satisfy isValidIdentRest)
    pure . Ident $ fromString (l:r)

atomOrPrimP :: Parser Value
atomOrPrimP = do
    i <- identP
    prims <- ask
    pure $ if i `elem` prims then Primop i else Atom i

applyP :: Parser Value
applyP = do
    fn:args <- valueP `sepBy1` spaceConsumer
    pure $ fn $$ args

listP :: Parser Value
listP = List <$> (char '\'' >> parensP (valueP `sepBy` spaceConsumer))

sortedMapP :: Parser Value
sortedMapP = do
    void $ symbol "sorted-map"
    SortedMap . Map.fromList <$> pairP `sepBy` spaceConsumer
  where
    pairP = (,) <$> identP <*> valueP

lambdaP :: Parser Value
lambdaP = do
    void $ symbol "lambda"
    vars <- parensP $ identP `sepBy` spaceConsumer
    body <- valueP
    pure $ Lambda vars body Nothing

refP :: Parser Value
refP = do
    void $ symbol "ref"
    Ref <$> identP

valueP :: Parser Value
valueP = do
  v <- choice
      [ stringLiteralP <?> "string"
      , boolLiteralP <?> "boolean"
      , atomOrPrimP <?> "identifier"
      , listP <?> "list"
      , parensP appLike <?> "application"
      ]
  spaceConsumer
  pure v
  where
    appLike = choice
        [ lambdaP <?> "lambda"
        , sortedMapP <?> "sorted-map"
        , refP <?> "ref"
        , applyP <?> "application"
        ]

-- * Utilities

-- | Parse and evaluate a Text.
--
-- Examples:
--
-- >>> interpret "test" "((lambda (x) x) #t)" pureEnv
-- Right (Boolean True)
--
-- >>> interpret "test" "(#t #f)" pureEnv
-- Left (TypeError "Trying to apply a non-function")
interpret :: Monad m => String -> Text -> Bindings m -> m (Either LangError Value)
interpret sourceName expr bnds = do
    let primopNames = Map.keys (bindingsPrimops bnds)
        parsed = runReader (runParserT (valueP <* eof) sourceName expr) primopNames
    case parsed of
        Left e  -> pure . Left $ ParseError e
        Right v -> runLang bnds $ eval v

-- | Parse and evaluate a Text as multiple expressions.
--
-- Examples:
--
-- >>> runLang pureEnv $ interpretMany "test" "(define id (lambda (x) x))\n(id #t)"
-- Right (Boolean True)
interpretMany :: Monad m => String -> Text -> Lang m Value
interpretMany sourceName src = do
    primopNames <- gets $ Map.keys . bindingsPrimops
    let parsed = parseValues sourceName src primopNames
    case partitionEithers parsed of
        ([], vs) -> last <$> mapM eval vs
        (e:_, _) -> throwError $ ParseError e

-- | Parse a Text as a series of values.
-- 'sourceName' is used for error reporting. 'prims' are the primop names.
--
-- Note that parsing continues even if one value fails to parse.
parseValues :: String -> Text -> [Ident] -> [Either (Par.ParseError Char Void) Value]
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
-- >>> parse "test" "#t" [] :: Either String Value
-- Right (Boolean True)
--
-- >>> parse "test" "hi" [identFromString "hi"] :: Either String Value
-- Right (Primop (Ident {fromIdent = "hi"}))
--
-- >>> parse "test" "hi" [] :: Either String Value
-- Right (Atom (Ident {fromIdent = "hi"}))
parse :: MonadError String m => String -> Text -> [Ident] -> m Value
parse file src ids = do
  let res = runReader (M.runParserT (valueP <* eof) file src) ids
  case res of
    Left err -> throwError $ M.parseErrorPretty' src err
    Right v  -> pure v

-- | Like 'parse', but uses "(test)" as the source name and the default set of
-- primops.
parseTest :: MonadError String m => Text -> m Value
parseTest t = parse "(test)" t (Map.keys $ bindingsPrimops e)
  where
    e :: Bindings (Lang Identity)
    e = pureEnv


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
