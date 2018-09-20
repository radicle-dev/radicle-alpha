module Radicle.Internal.Parse where

import           Protolude hiding (SrcLoc, try)

import           Data.Char (isAlphaNum, isLetter)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import qualified Data.Text as T
import           GHC.Exts (IsString(..))
import           Text.Megaparsec
                 ( ParsecT
                 , State(..)
                 , between
                 , choice
                 , defaultTabWidth
                 , eof
                 , getPosition
                 , initialPos
                 , manyTill
                 , runParserT'
                 , sepBy
                 , try
                 , (<?>)
                 )
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char (char, satisfy, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core

-- * The parser

type Parser a = ParsecT Void Text (Reader [Ident]) a
type VParser = Parser Value

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";;"
    blockComment = L.skipBlockComment "#|" "|#" -- R6RS

tag :: ValueF Value -> VParser
tag v = do
    pos <- getPosition -- N.B. in megaparsec 7 this is called getSourcePos
    pure $ Ann.Annotated (Ann.WithPos (Ann.SrcPos pos) v)


symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

inside :: Text -> Text -> Parser a -> Parser a
inside b e = between (symbol b >> spaceConsumer) (spaceConsumer >> symbol e)

parensP :: Parser a -> Parser a
parensP = inside "(" ")"

bracesP :: Parser a -> Parser a
bracesP = inside "{" "}"

stringLiteralP :: VParser
stringLiteralP = lexeme $
    tag =<< StringF . toS <$> (char '"' >> manyTill L.charLiteral (char '"'))

boolLiteralP :: VParser
boolLiteralP = lexeme $ tag =<< BooleanF <$> (char '#' >>
        (char 't' >> pure True) <|> (char 'f' >> pure False))

numLiteralP :: VParser
numLiteralP = tag =<< NumberF <$> signed L.scientific
  where
    -- We don't allow spaces between the sign and digits so that we can remain
    -- consistent with the general Scheme of things.
    signed p = M.option identity ((identity <$ char '+') <|> (negate <$ char '-')) <*> p

identP :: Parser Ident
identP = lexeme $ do
    l <- satisfy isValidIdentFirst
    r <- many (satisfy isValidIdentRest)
    pure . Ident $ fromString (l:r)

atomOrPrimP :: VParser
atomOrPrimP = do
    i <- identP
    prims <- ask
    tag $ if i `elem` prims then PrimopF i else AtomF i

keywordP :: VParser
keywordP = do
  _ <- char ':'
  kw <- many (satisfy isValidIdentRest)
  tag . KeywordF . Ident . fromString $ kw

listP :: VParser
listP = parensP (tag =<< (ListF <$> valueP `sepBy` spaceConsumer))

dictP :: VParser
dictP = bracesP (tag =<< (DictF . Map.fromList <$> evenItems))
  where
    evenItems = twoItems `sepBy` spaceConsumer
    twoItems = do
      x <- valueP
      spaceConsumer
      y <- valueP
      pure (x,y)

quoteP :: VParser
quoteP = do
    val <- char '\'' >> valueP
    q <- tag $ PrimopF (unsafeToIdent "quote")
    pure $ List [q, val]

valueP :: VParser
valueP = do
  v <- choice
      [ stringLiteralP <?> "string"
      , boolLiteralP <?> "boolean"
      , keywordP <?> "keyword"
      , try numLiteralP <?> "number"
      , atomOrPrimP <?> "identifier"
      , quoteP <?> "quote"
      , listP <?> "list"
      , dictP <?> "dict"
      ]
  spaceConsumer
  pure v

-- * Utilities

-- | Parse a Text as a series of values.
--
-- Note that parsing continues even if one value fails to parse.
parseValues
    :: Text    -- ^ Name of source file (for error reporting)
    -> Text    -- ^ Source code to be parsed
    -> [Ident] -- ^ Primop identifiers
    -> [Either (Par.ParseError Char Void) Value]
parseValues sourceName srcCode prims = withoutLeadingSpaces
  where
    initial = State
        { stateInput = srcCode
        , statePos = initialPos (toS sourceName) :| []
        , stateTokensProcessed = 0
        , stateTabWidth = defaultTabWidth
        }
    withoutLeadingSpaces =
      let (s', _) = runReader (runParserT' spaceConsumer initial) prims
      in if T.null (stateInput s') then [] else go s'
    go s = let (s', v) = runReader (runParserT' valueP s) prims
           in if T.null (stateInput s') then [v] else v:go s'

-- | Parse a single value.
--
-- Examples:
--
-- >>> untag <$> parse "test" "#t" [] :: Either Text UntaggedValue
-- Right (Annotated (Identity (BooleanF True)))
--
-- >>> untag <$> parse "test" "hi" [unsafeToIdent "hi"] :: Either Text UntaggedValue
-- Right (Annotated (Identity (PrimopF (Ident {fromIdent = "hi"}))))
--
-- >>> untag <$> parse "test" "hi" [] :: Either Text UntaggedValue
-- Right (Annotated (Identity (AtomF (Ident {fromIdent = "hi"}))))
parse :: MonadError Text m
    => Text    -- ^ Name of source file (for error reporting)
    -> Text    -- ^ Source code to be parsed
    -> [Ident] -- ^ Primop identifiers
    -> m Value
parse file src ids = do
  let res = runReader (M.runParserT (spaceConsumer *> valueP <* eof) (toS file) src) ids
  case res of
    Left err -> throwError . toS $ M.parseErrorPretty' src err
    Right v  -> pure v

-- ** Valid identifiers
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

extendedChar :: [Char]
extendedChar = ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<' , '=', '>'
  , '?', '@', '^', '_', '~']
