{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Internal.Core where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.Reader (MonadReader, Reader, ask, local,
                                       runReader)
import           Data.Data (Data)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Semigroup (Semigroup, (<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Exts (IsList(..), IsString)
import           GHC.Generics (Generic)
import qualified Text.Megaparsec.Error as Par

-- * Value

-- | An error throw during parsing or evaluating expressions in the language.
data LangError =
      UnknownIdentifier Ident
    | Impossible Text
    | TypeError Text
    | WrongNumberOfArgs
          Text   -- ^ function name
          Int    -- ^ expected
          Int    -- ^ actual
    | OtherError Text
    | ParseError (Par.ParseError Char Void)
    deriving (Eq, Show, Read, Generic)

-- | An identifier in the language.
--
-- Not all Texts are valid identifiers, so you should not be using the Ident
-- constructor directly. Instead, use makeIdent from Oscoin.Language
newtype Ident = Ident { fromIdent :: Text }
    deriving (Eq, Show, Read, Ord, Generic, IsString, Data)


-- | An expression or value in the language.
data Value =
      Atom Ident
    | String Text
    | Boolean Bool
    | List [Value]
    | Primop Ident
    | Apply Value [Value]
    | SortedMap (Map.Map Ident Value)
    -- | Since there are no side-effects, there is no point having a list of
    -- values for the body of a lambda.
    | Lambda
        [Ident]     -- ^ parameters
        Value       -- ^ body.
        (Maybe Env) -- ^ closure
    deriving (Eq, Ord, Show, Read, Generic, Data)

-- | The environment, which keeps all known bindings.
newtype Env = Env { fromEnv :: Map Ident Value }
    deriving (Eq, Show, Ord, Read, Semigroup, Monoid, Generic, Data)

instance IsList Env where
    type Item Env = (Ident, Value)
    fromList = Env . fromList
    toList = GHC.Exts.toList . fromEnv

-- | Set the value of an identifier in the Env.
setEnv :: Ident -> Value -> Env -> Env
setEnv i v (Env e) = Env $ Map.insert i v e

-- | The environment in which expressions are evaluated. We use Reader rather
-- than State since modifications to the environment (modulo lambdas, which use
-- 'local') are factored out.
newtype LangM a = LangM
    { fromLangM :: ExceptT LangError (Reader Env) a }
    deriving (Functor, Applicative, Monad, MonadError LangError, MonadReader Env)

runLangM :: Env -> LangM a -> Either LangError a
runLangM e l = runReader (runExceptT $ fromLangM l) e

-- | Lookup an atom in the environment
lookupAtom :: Ident -> LangM Value
lookupAtom i = ask >>= \(Env env) -> case Map.lookup i env of
    Nothing -> throwError $ UnknownIdentifier i
    Just v  -> pure v

-- | Lookup a primop.
lookupPrimop :: Ident -> LangM ([Value] -> LangM Value)
lookupPrimop i = case Map.lookup i primops of
    Nothing -> throwError $ Impossible "Unknown primop"
    Just v  -> pure v

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable.
primops :: Map Ident ([Value] -> LangM Value)
primops = Map.fromList
    [ ("eval", \args -> case args of
          [List (v:vs)] -> eval (Apply v vs)
          [Apply fn v ] -> eval =<< Apply <$> eval fn <*> traverse eval v
          [x]           -> eval x
          x             -> throwError $
            OtherError . T.pack $ "'eval' requires list as argument: " <> show x)
    , ("quote", \args -> case args of
          [v] -> pure v
          xs  -> throwError $ WrongNumberOfArgs "quote" 1 (length xs))
    , ("define", \args -> case args of
          [Atom name, val] -> do
              val' <- eval val
              pure $ List [Atom "set!", Atom name, val']
          [_, _] -> throwError $ OtherError "define expects atom for first arg"
          xs          -> throwError $ WrongNumberOfArgs "define" 2 (length xs))
    , ("string?", \args -> case args of
          [String _] -> pure $ Boolean True
          [_]        -> pure $ Boolean False
          xs         -> throwError $ WrongNumberOfArgs "string?" 1 (length xs))
    , ("boolean?", \args -> case args of
          [Boolean _] -> pure $ Boolean True
          [_]         -> pure $ Boolean False
          xs          -> throwError $ WrongNumberOfArgs "boolean?" 1 (length xs))
    , ("if", \args -> case args of
          [cond, t, f] -> do
            b <- eval cond
            -- I hate this as much as everyone that might ever read Haskell, but
            -- in Lisps a lot of things that one might object to are True...
            if b == Boolean False then eval f else eval t
          xs -> throwError $ WrongNumberOfArgs "if" 3 (length xs))
    ]

-- * Eval

-- | Evaluate a Value.
eval :: Value -> LangM Value
eval val = case val of
    Atom i -> lookupAtom i
    List vals -> List <$> traverse eval vals
    String s -> pure $ String s
    Boolean b -> pure $ Boolean b
    Apply mfn vs -> do
        mfn' <- eval mfn
        case mfn' of
            Primop i -> do
                fn <- lookupPrimop i
                -- Primops get to decide whether and how their args are
                -- evaluated.
                fn vs
            Lambda _ _ Nothing -> throwError $ Impossible
                "lambda should already have an env"
            Lambda bnds body (Just closure) -> do
                  vs' <- traverse eval vs
                  let mappings = GHC.Exts.fromList (zip bnds vs')
                      modEnv = const $ mappings <> closure
                  local modEnv (eval body)
            _ -> throwError $ TypeError "Trying to apply a non-function"
    Primop i -> pure $ Primop i
    e@(Lambda _ _ (Just _)) -> pure e
    Lambda args body Nothing -> Lambda args body . Just <$> ask
    SortedMap mp -> do
        let evalSnd (a,b) = (a ,) <$> eval b
        SortedMap . Map.fromList <$> traverse (evalSnd) (Map.toList mp)

-- * Helpers

-- | Infix function application
infixr 1 $$
($$) :: Value -> [Value] -> Value
($$) = Apply
