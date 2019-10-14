module Radicle.Lang.Interpret where

import           Protolude

import qualified Radicle.Lang.Annotation as Ann
import           Radicle.Lang.Core
import           Radicle.Lang.Eval
import           Radicle.Lang.Parse

-- | Parse and evaluate a Text.
--
-- Examples:
--
-- >>> import Control.Monad.Identity
-- >>> import Radicle.Lang.PrimFns
-- >>> fmap Ann.untag . runIdentity $ interpret "test" "((fn [x] x) #t)" pureEnv
-- Right (Annotated (Identity (BooleanF True)))
--
-- >>> import Control.Monad.Identity
-- >>> case noStack . runIdentity $ interpret "test" "(#t #f)" pureEnv of { Left (NonFunctionCalled v) -> Ann.untag v }
-- Annotated (Identity (BooleanF True))
interpret
    :: Monad m
    => Text                 -- ^ Name of source file (for error reporting)
    -> Text                 -- ^ Source code to be interpreted
    -> Bindings (PrimFns m) -- ^ Bindings to be used
    -> m (Either (LangError Value) Value)
interpret sourceName expr bnds
    = fst <$> interpretWithState sourceName expr bnds

-- | Like 'interpret', but also returns the resulting state.
interpretWithState
    :: Monad m
    => Text                 -- ^ Name of source file (for error reporting)
    -> Text                 -- ^ Source code to be interpreted
    -> Bindings (PrimFns m) -- ^ Bindings to be used
    -> m (Either (LangError Value) Value, Bindings (PrimFns m))
interpretWithState sourceName expr bnds = do
    let parsed = parseValue (toS sourceName) expr
    case parsed of
        Left e  -> pure (Left (LangError [Ann.thisPos] (ParseError e)), bnds)
        Right v -> runLang bnds (eval v)

-- | Parse and evaluate a Text as multiple expressions.
--
-- The first argument is the name of the source file to be used for error
-- reporting.
--
-- Examples:
--
-- >>> import Radicle.Lang.PrimFns
-- >>> fmap (fmap Ann.untag . fst) <$> runLang pureEnv $ interpretMany "test" "(def id (fn [x] x))\n(id #t)"
-- Right (Annotated (Identity (BooleanF True)))
interpretMany
    :: Monad m
    => Text  -- ^ Name of source file (for error reporting)
    -> Text  -- ^ Source code to be interpreted
    -> Lang m Value
interpretMany sourceName src = case parseValues sourceName src of
    Left err -> throwErrorHere $ ParseError err
    Right vs -> do
        es <- mapM eval vs
        case lastMay es of
            Just e -> pure e
            _ -> throwErrorHere
               $ OtherError "interpretMany should be called with at least one expression."
