module Radicle.Internal.Parse where

import           Protolude hiding (SrcLoc, try)

import           Control.Monad.Fail
import           Control.Monad.Morph (generalize, hoist)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Exts (IsString(..))
import           Text.Megaparsec
                 ( ParsecT
                 , between
                 , choice
                 , eof
                 , getSourcePos
                 , manyTill
                 , satisfy
                 , sepBy
                 , try
                 , (<?>)
                 )
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Identifier

-- * The parser

data InShortLambda = NotInShortLambda | InShortLambda
  deriving Eq

-- | A parser. In order to detect nested short-lambdas, the parser keeps track
-- of if it has entered a short-lambda.
type Parser a = ParsecT Void Text (Reader InShortLambda) a
type VParser = Parser Value

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";;"
    blockComment = L.skipBlockComment "#|" "|#" -- R6RS

tag :: ValueF Value -> VParser
tag v = do
    pos <- getSourcePos
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
stringLiteralP = lexeme $ tag =<< StringF . toS <$> escapedString
  where
    escapedString = catMaybes <$> (char '"' >> manyTill ch (char '"'))
    ch = (Just <$> L.charLiteral) <|> (Nothing <$ string "\\&")

boolLiteralP :: VParser
boolLiteralP = lexeme $ tag =<< BooleanF <$> (char '#' >>
        (char 't' >> pure True) <|> (char 'f' >> pure False))

numLiteralP :: VParser
numLiteralP = tag =<< NumberF <$> signed pos
  where
    posrat =
      do n <- L.decimal
         _ <- char '/'
         d <- L.decimal
         pure (n % d)

    pos = try posrat <|> fromIntegral <$> (L.decimal :: Parser Integer)

    -- We don't allow spaces between the sign and digits so that we can remain
    -- consistent with the general Scheme of things.
    signed p = M.option identity ((identity <$ char '+') <|> (negate <$ char '-')) <*> p

identP :: Parser Ident
identP = lexeme $ divSym <|> namespaced <|> (Unnamespaced <$> unnamespaced)
  where
    divSym = char '/' >> pure (NakedT "/")
    simple1 = do
      l <- satisfy isValidIdentFirst
      r <- many (satisfy isValidIdentRest)
      pure . Naked . fromString $ l:r
    simple2 = do
      r <- many (satisfy isValidIdentRest)
      pure . Naked . fromString $ r
    namespaced = try $ do
      n <- simple1
      _ <- string "//"
      Namespaced n <$> unnamespaced
    unnamespaced = try $ qualified <|> (NakedU <$> simple1)
    qualified = try $ do
      q <- simple1
      _ <- char '/'
      Qualified q <$> simple2
                
atomP :: VParser
atomP = tag . AtomF =<< identP

keywordP :: VParser
keywordP = do
  _ <- char ':'
  i <- identP
  tag . KeywordF $ i

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
    q <- tag $ AtomF (NakedT "quote")
    pure $ List [q, val]

data QMark = QMPlain | QMDigit Int
  deriving (Eq, Ord)

shortLam :: VParser
shortLam = do
    _ <- char '\\'
    inShortAlready <- ask
    if inShortAlready == InShortLambda
      then fail "Short-lambdas cannot be nested."
      else do
        expr <- local (const InShortLambda) valueP
        case qMarks expr of
          Nothing -> fail "Invalid ?-atoms in short-lambda: a '?' may only be followed by a single non-zero digit."
          Just qs -> case validQMarks (Set.toList qs) of
            Just args -> tag . ListF $ [Atom (NakedT "fn"), Vec (Seq.fromList args), expr]
            Nothing -> fail "Invalid ?-atoms in short-lambda: plain `?` and numbered `?` cannot be used at the same time."
  where
    -- | A short-lambda should either use no ?-atoms at all, only plain ?-atoms,
    -- or only numbered ?-atoms.
    validQMarks :: [QMark] -> Maybe [Value]
    validQMarks qs = case (qs, traverse numbered qs) of
      ([], _)        -> Just []
      (_, Just ds)   -> Just $ qMarkAtom . QMDigit <$> [1..maximum ds]
      ([QMPlain], _)-> Just [qMarkAtom QMPlain]
      _              -> Nothing

    -- | Get the ?-atoms in an expression (?-atoms are either an @?@, or @?i@
    -- where @i@ in @{1, ..., 9}@.). If there are any atoms which start with a
    -- @?@ followed by digits that are /not/ ?-atoms, then returns Nothing.
    qMarks :: Value -> Maybe (Set QMark)
    qMarks = \case
      Atom (NakedT t) -> case T.uncons t of
        Just ('?', rest) | T.all Char.isDigit rest ->
          case T.uncons rest of
            Just (d, rest') | d /= '0' && T.null rest' -> Just (Set.singleton (QMDigit (Char.digitToInt d)))
            Just _ -> Nothing
            Nothing -> Just (Set.singleton QMPlain)
        _ -> Just Set.empty
      List xs -> coll xs
      Vec xs -> coll xs
      Dict m -> Set.union <$> coll (Map.keys m) <*> coll (Map.elems m)
      _ -> pure Set.empty

    coll xs = Set.unions <$> traverse qMarks xs

    qMarkAtom QMPlain     = Atom (NakedT "?")
    qMarkAtom (QMDigit i) = Atom (NakedT ("?" <> show i))

    numbered = \case
      QMPlain -> Nothing
      QMDigit d -> Just d

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
      , shortLam <?> "short-lambda"
      ]
  spaceConsumer
  pure v

-- * Utilities

-- | Parses a top-level expression (i.e. assumes not inside a short-lambda).
parseTop :: Parser a -> Text -> Text -> Either (Par.ParseErrorBundle Text Void) a
parseTop p src code = runIdentity $ runReaderT (M.runParserT p (toS src) code) NotInShortLambda

parseValue :: Text -> Text -> Either (Par.ParseErrorBundle Text Void) Value
parseValue = parseTop (spaceConsumer *> valueP <* eof)

-- | Attempts to parse a value. If input is still left unconsumed, calls the
-- provided `m (Maybe Text)` to get more text. This allows for multiline inputs
-- in the REPL.
--
-- Megaparsec doesn't yet have streaming support (see
-- <https://github.com/mrkkrp/megaparsec/issues/332 this issue>), so we reparse
-- from scratch if more input is needed, which is somewhat inefficient.
parseREPL
    :: Monad m
    => m (Maybe Text)
    -> Text
    -> m (Either [Char] Value)
parseREPL getter code
    = runReaderT (go code) NotInShortLambda
  where
    isEOFError e = case e of
        Par.TrivialError _ (Just Par.EndOfInput) _ -> True
        _                                          -> False
    go this = do
        res <- hoist generalize $ M.runParserT (spaceConsumer *> valueP) "[repl]" this
        case res of
            Left errs ->
                if any isEOFError (Par.bundleErrors errs)
                    then do
                        next <- lift getter
                        case next of
                            Nothing    -> pure . Left $ Par.errorBundlePretty errs
                            Just next' -> go $ this <> "\n" <> next'
                    else pure . Left $ Par.errorBundlePretty errs
            Right v -> pure $ Right v

-- | Parse a Text as a series of values.
--
-- Note that parsing continues even if one value fails to parse.
parseValues
    :: Text    -- ^ Name of source file (for error reporting)
    -> Text    -- ^ Source code to be parsed
    -> Either (Par.ParseErrorBundle Text Void) [Value]
parseValues sourceName srcCode
    = parseTop (spaceConsumer *> many valueP <* eof) sourceName srcCode

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
  let res = parseValue file src
  case res of
    Left err -> throwError . toS $ M.errorBundlePretty err
    Right v  -> pure v

-- | Smart constructor for Ident.
mkIdent :: Text -> Maybe Ident
mkIdent t = case parseTop (valueP <* M.eof) "" t of
    -- We use the 'valueP' parser instead of 'identP' so that we don’t
    -- negative numbers  like @-4@.
    Right (Atom i) -> pure i
    _              -> Nothing
