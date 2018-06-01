{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Radicle.Internal.Core where

import           Control.Monad.Except (ExceptT(..), MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State
import           Data.Bifunctor (first)
import           Data.Data ((:~:)(Refl), Data, cast, eqT)
import           Data.Generics (everywhereM)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Semigroup (Semigroup, (<>))
import           Data.Text (Text)
import           Data.Void (Void)
import           GHC.Exts (IsList(..), fromString)
import           GHC.Generics (Generic)
import qualified Text.Megaparsec.Error as Par


-- * Value

-- | An error throw during parsing or evaluating expressions in the language.
data LangError =
      UnknownIdentifier Ident
    | Impossible Text
    | TypeError Text
    -- | Takes the function name, expected number of args, and actual number of
    -- args
    | WrongNumberOfArgs Text Int Int
    | OtherError Text
    | ParseError (Par.ParseError Char Void)
    deriving (Eq, Show, Read, Generic)


-- | An expression or value in the language.
data Value =
    -- | A regular (hyperstatic) variable.
      Atom Ident
    | String Text
    | Boolean Bool
    | List [Value]
    | Primop Ident
    | Apply Value [Value]
    | SortedMap (Map.Map Ident Value)
    -- | A variable that must be looked up at the *call-site*.
    | Ref Ident
    -- | Since there are no side-effects, there is no point having a list of
    -- values for the body of a lambda.
    --
    -- Takes the arguments/parameters, a body, and possibly a closure.
    | Lambda [Ident] Value (Maybe Env)
    deriving (Eq, Ord, Show, Read, Generic, Data)

-- | An identifier in the language.
--
-- Not all `Text`s are valid identifiers, so we do not export the constructor.
-- Instead, use `makeIdent`.
newtype Ident = Ident { fromIdent :: Text }
    deriving (Eq, Show, Read, Ord, Generic, Data)

-- Unsafe! Only use this if you know the string at compile-time and know it's a
-- valid identifier
toIdent :: String -> Ident
toIdent = Ident . fromString

-- | The environment, which keeps all known bindings.
newtype Env = Env { fromEnv :: Map Ident Value }
    deriving (Eq, Show, Ord, Read, Semigroup, Monoid, Generic, Data)

instance IsList Env where
    type Item Env = (Ident, Value)
    fromList = Env . fromList
    toList = GHC.Exts.toList . fromEnv

-- | Primop mappings. The parameter specifies the monad the primops run in.
type Primops m = Map Ident ([Value] -> LangT (Bindings m) m Value)

-- | Bindings, either from the env or from the primops.
data Bindings m = Bindings
    { bindingsEnv     :: Env
    , bindingsPrimops :: Primops m
    } deriving (Generic)

-- | The environment in which expressions are evaluated.
newtype LangT r m a = LangT
    { fromLangT :: ExceptT LangError (StateT r m) a }
    deriving (Functor, Applicative, Monad, MonadError LangError, MonadState r)

instance MonadTrans (LangT r) where lift = LangT . lift . lift

-- | A monad for language operations specialized to have as state the Bindings
-- with appropriate underlying monad.
type Lang m = LangT (Bindings m) m

runLang :: Monad m => Bindings m -> Lang m a -> m (Either LangError a)
runLang e l = evalStateT (runExceptT $ fromLangT l) e

-- | Like 'local' or 'withState'
withEnv :: Monad m => (Bindings m -> Bindings m) -> Lang m a -> Lang m a
withEnv modifier action = do
    oldEnv <- get
    modify modifier
    res <- action
    put oldEnv
    pure res

-- * Functions

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: Monad m => Bindings m
pureEnv = Bindings e purePrimops
  where
    e = fromList [(toIdent "eval", Primop $ toIdent "base-eval")]

addBinding :: Monad m => Ident -> Value -> Bindings m -> Bindings m
addBinding i v b = b
    { bindingsEnv = Env . Map.insert i v . fromEnv $ bindingsEnv b }

-- | Lookup an atom in the environment
lookupAtom :: Monad m => Ident -> Lang m Value
lookupAtom i = get >>= \e -> case Map.lookup i . fromEnv $ bindingsEnv e of
    Nothing -> throwError $ UnknownIdentifier i
    Just v  -> pure v

-- | Lookup a primop.
lookupPrimop :: Monad m => Ident -> Lang m ([Value] -> Lang m Value)
lookupPrimop i = get >>= \e -> case Map.lookup i $ bindingsPrimops e of
    Nothing -> throwError $ Impossible "Unknown primop"
    Just v  -> pure v

defineAtom :: Monad m => Ident -> Value -> Lang m ()
defineAtom i v = modify $ addBinding i v

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable via 'define'.
purePrimops :: Monad m => Primops m
purePrimops = Map.fromList $ first toIdent <$>
    [ ("base-eval", \args -> case args of
          [x] -> baseEval x
          xs  -> throwError $ WrongNumberOfArgs "base-eval" 1 (length xs))
    , ("quote", \args -> case args of
          [v] -> pure v
          xs  -> throwError $ WrongNumberOfArgs "quote" 1 (length xs))
    , ("define", \args -> case args of
          [Atom name, val] -> do
              val' <- eval val
              defineAtom name val'
              pure nil
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
    , ("deref-all", \args -> do
          let derefOne :: forall m b. (Monad m, Data b) => (b -> Lang m b)
              derefOne x = case (cast x, eqT :: Maybe (b :~: Value)) of
                  (Just (Ref i), Just Refl) -> lookupAtom i
                  _                         -> pure x
          case args of
              [x] -> eval x >>= everywhereM derefOne
              xs  -> throwError $ WrongNumberOfArgs "deref-all" 1 (length xs))
    ]

-- * Eval

-- | The buck-passing eval. Uses whatever 'eval' is in scope.
eval :: Monad m => Value -> Lang m Value
eval val = do
    e <- lookupAtom (toIdent "eval")
    case e of
        Primop i -> do
            fn <- lookupPrimop i
            -- Primops get to decide whether and how their args are
            -- evaluated.
            fn [val]
        Lambda _ _ Nothing -> throwError $ Impossible
            "lambda should already have an env"
        Lambda [bnd] body (Just closure) -> do
              let mappings = fromList [(bnd, val)]
                  modEnv = mappings <> closure
              withEnv (\e' -> e' { bindingsEnv = modEnv}) (eval body)
        _ -> throwError $ TypeError "Trying to apply a non-function"

-- | The built-in, original, eval.
baseEval :: Monad m => Value -> Lang m Value
baseEval val = case val of
    Atom i -> lookupAtom i
    Ref i -> pure $ Ref i
    List vals -> List <$> traverse baseEval vals
    String s -> pure $ String s
    Boolean b -> pure $ Boolean b
    Apply mfn vs -> do
        mfn' <- baseEval mfn
        case mfn' of
            Primop i -> do
                fn <- lookupPrimop i
                -- Primops get to decide whether and how their args are
                -- evaluated.
                fn vs
            Lambda _ _ Nothing -> throwError $ Impossible
                "lambda should already have an env"
            Lambda bnds body (Just closure) -> do
                  vs' <- traverse baseEval vs
                  let mappings = fromList (zip bnds vs')
                      modEnv = mappings <> closure
                  withEnv (\e -> e { bindingsEnv = modEnv }) (baseEval body)
            _ -> throwError $ TypeError "Trying to apply a non-function"
    Primop i -> pure $ Primop i
    e@(Lambda _ _ (Just _)) -> pure e
    Lambda args body Nothing -> gets $ Lambda args body . Just . bindingsEnv
    SortedMap mp -> do
        let evalSnd (a,b) = (a ,) <$> baseEval b
        SortedMap . Map.fromList <$> traverse evalSnd (Map.toList mp)



-- * Helpers

-- | Infix function application
infixr 1 $$
($$) :: Value -> [Value] -> Value
($$) = Apply

nil :: Value
nil = List []
