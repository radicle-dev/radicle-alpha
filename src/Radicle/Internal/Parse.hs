module Radicle.Internal.Parse where

import           Protolude hiding (SrcLoc, try)

import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
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
import           Text.Megaparsec.Char (char, satisfy, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Identifier

-- * The parser

type Parser a = ParsecT Void Text Identity a
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

sqBracketsP :: Parser a -> Parser a
sqBracketsP = inside "[" "]"

-- Parsing of string literals handles escape sequences just like Haskell does.
stringLiteralP :: VParser
stringLiteralP = lexeme $ tag =<< StringF . toS <$> escaptedString
  where
    escapedString = catMaybes <$> (char '"' >> manyTill ch (char '"'))
    ch = (Just <$> L.charLiteral) <|> (Nothing <$ string "\\&")

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

atomP :: VParser
atomP = tag . AtomF =<< identP

keywordP :: VParser
keywordP = do
  _ <- char ':'
  kw <- many (satisfy isValidIdentRest)
  tag . KeywordF . Ident . fromString $ kw

listP :: VParser
listP = parensP (tag =<< (ListF <$> valueP `sepBy` spaceConsumer))

vecP :: VParser
vecP = sqBracketsP (tag =<< (VecF . Seq.fromList <$> valueP `sepBy` spaceConsumer))

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
    q <- tag $ AtomF (unsafeToIdent "quote")
    pure $ List [q, val]

valueP :: VParser
valueP = do
  v <- choice
      [ stringLiteralP <?> "string"
      , boolLiteralP <?> "boolean"
      , keywordP <?> "keyword"
      , try numLiteralP <?> "number"
      , atomP <?> "identifier"
      , quoteP <?> "quote"
      , listP <?> "list"
      , vecP <?> "vector"
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
    -> [Either (Par.ParseError Char Void) Value]
parseValues sourceName srcCode = withoutLeadingSpaces
  where
    initial = State
        { stateInput = srcCode
        , statePos = initialPos (toS sourceName) :| []
        , stateTokensProcessed = 0
        , stateTabWidth = defaultTabWidth
        }
    withoutLeadingSpaces =
      let (s', _) = runIdentity (runParserT' spaceConsumer initial)
      in if T.null (stateInput s') then [] else go s'
    go s = let (s', v) = runIdentity (runParserT' valueP s)
           in if T.null (stateInput s') then [v] else v:go s'

-- | Parse a single value.
--
-- Examples:
--
-- >>> untag <$> parse "test" "#t" :: Either Text UntaggedValue
-- Right (Annotated (Identity (BooleanF True)))
--
-- >>> untag <$> parse "test" "hi" :: Either Text UntaggedValue
-- Right (Annotated (Identity (AtomF (Ident {fromIdent = "hi"}))))
parse :: MonadError Text m
    => Text    -- ^ Name of source file (for error reporting)
    -> Text    -- ^ Source code to be parsed
    -> m Value
parse file src = do
  let res = runIdentity (M.runParserT (spaceConsumer *> valueP <* eof) (toS file) src)
  case res of
    Left err -> throwError . toS $ M.parseErrorPretty' src err
    Right v  -> pure v
