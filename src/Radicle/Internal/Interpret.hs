module Radicle.Internal.Interpret where

import           Protolude

import qualified Data.Map.Strict as Map
import           Text.Megaparsec (eof, runParserT)

import           Radicle.Internal.Core
import           Radicle.Internal.Parse

-- | Parse and evaluate a Text.
--
-- Examples:
--
-- >>> import Control.Monad.Identity
-- >>> import Radicle.Internal.Primops
-- >>> runIdentity $ interpret "test" "((lambda (x) x) #t)" pureEnv
-- Right (Boolean True)
--
-- >>> import Control.Monad.Identity
-- >>> runIdentity $ interpret "test" "(#t #f)" pureEnv
-- Left (TypeError "Trying to apply a non-function")
interpret
    :: Monad m
    => Text                 -- ^ Name of source file (for error reporting)
    -> Text                 -- ^ Source code to be interpreted
    -> Bindings (Primops m) -- ^ Bindings to be used
    -> m (Either (LangError Value) Value)
interpret sourceName expr bnds = do
    let primopNames = Map.keys (getPrimops $ bindingsPrimops bnds)
        parsed = runReader (runParserT (spaceConsumer *> valueP <* eof) (toS sourceName) expr) primopNames
    case parsed of
        Left e  -> pure . Left $ ParseError e
        Right v -> fst <$> runLang bnds (eval v)

-- | Parse and evaluate a Text as multiple expressions.
--
-- The first argument is the name of the source file to be used for error
-- reporting.
--
-- Examples:
--
-- >>> import Radicle.Internal.Primops
-- >>> fmap fst <$> runLang pureEnv $ interpretMany "test" "(define id (lambda (x) x))\n(id #t)"
-- Right (Boolean True)
interpretMany
    :: Monad m
    => Text  -- ^ Name of source file (for error reporting)
    -> Text  -- ^ Source code to be interpreted
    -> Lang m Value
interpretMany sourceName src = do
    primopNames <- gets $ Map.keys . getPrimops . bindingsPrimops
    let parsed = parseValues sourceName src primopNames
    case partitionEithers parsed of
        ([], vs) -> do
          es <- mapM eval vs
          case lastMay es of
             Just e -> pure e
             _ -> throwError
                $ OtherError "InterpretMany should be called with at least one expression."
        (e:_, _) -> throwError $ ParseError e
