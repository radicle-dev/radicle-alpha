module Radicle.Internal.Interpret where

import           Protolude

import           Text.Megaparsec (eof, runParserT)

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Parse

-- | Parse and evaluate a Text.
--
-- Examples:
--
-- >>> import Control.Monad.Identity
-- >>> import Radicle.Internal.Primops
-- >>> fmap Ann.untag . runIdentity $ interpret "test" "((fn [x] x) #t)" pureEnv
-- Right (Annotated (Identity (BooleanF True)))
--
-- >>> import Control.Monad.Identity
-- >>> noStack . runIdentity $ interpret "test" "(#t #f)" pureEnv
-- Left (TypeError "Trying to apply a non-function")
interpret
    :: Monad m
    => Text                 -- ^ Name of source file (for error reporting)
    -> Text                 -- ^ Source code to be interpreted
    -> Bindings (PrimFns m) -- ^ Bindings to be used
    -> m (Either (LangError Value) Value)
interpret sourceName expr bnds = do
    let parsed = runIdentity (runParserT (spaceConsumer *> valueP <* eof) (toS sourceName) expr)
    case parsed of
        Left e  -> pure . Left $ LangError [Ann.thisPos] (ParseError e)
        Right v -> fst <$> runLang bnds (eval v)

-- | Parse and evaluate a Text as multiple expressions.
--
-- The first argument is the name of the source file to be used for error
-- reporting.
--
-- Examples:
--
-- >>> import Radicle.Internal.Primops
-- >>> fmap (fmap Ann.untag . fst) <$> runLang pureEnv $ interpretMany "test" "(def id (fn [x] x))\n(id #t)"
-- Right (Annotated (Identity (BooleanF True)))
interpretMany
    :: Monad m
    => Text  -- ^ Name of source file (for error reporting)
    -> Text  -- ^ Source code to be interpreted
    -> Lang m Value
interpretMany sourceName src = do
    let parsed = parseValues sourceName src
    case partitionEithers parsed of
        ([], vs) -> do
          es <- mapM eval vs
          case lastMay es of
             Just e -> pure e
             _ -> throwErrorHere
                $ OtherError "InterpretMany should be called with at least one expression."
        (e:_, _) -> throwErrorHere $ ParseError e
