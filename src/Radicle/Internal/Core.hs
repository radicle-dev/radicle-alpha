{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Radicle.Internal.Core where

import           Control.Monad.Except (ExceptT(..), MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State
import           Data.Bifunctor (first)
import           Data.Data ((:~:)(Refl), Data, cast, eqT)
import           Data.Generics (everywhereM)
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
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
    | Exit
    deriving (Eq, Show, Read, Generic)


-- | An expression or value in the language.
data Value =
    -- | A regular (hyperstatic) variable.
      Atom Ident
    | String Text
    | Number Scientific
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
purePrimops :: forall m. Monad m => Primops m
purePrimops = Map.fromList $ first Ident <$>
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
    , ("eq?", evalArgs $ \args -> case args of
          [a, b] -> fmap Boolean $ (==) <$> eval a <*> eval b
          xs     -> throwError $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("cons", evalArgs $ \args -> case args of
          [x, List xs] -> pure $ List (x:xs)
          [_, _]       -> throwError $ TypeError "cons: second argument must be list"
          xs           -> throwError $ WrongNumberOfArgs "cons" 2 (length xs))
    , ("car", evalArgs $ \args -> case args of
          [List (x:_)] -> eval x
          [List []]    -> throwError $ OtherError "car: empty list"
          [_]          -> throwError $ TypeError "car: expects list argument"
          xs           -> throwError $ WrongNumberOfArgs "car" 1 (length xs))
    , ("cdr", evalArgs $ \args -> case args of
          [List (_:xs)] -> eval $ List xs
          [List []]     -> throwError $ OtherError "cdr: empty list"
          [_]           -> throwError $ TypeError "cdr: expects list argument"
          xs            -> throwError $ WrongNumberOfArgs "cdr" 1 (length xs))
    , ("lookup", \args -> case args of
          [Atom a, SortedMap m] -> case Map.lookup a m of
              Just v  -> eval v
              -- Probably an exception is better, but that seems cruel when you
              -- have no exception handling facilities.
              Nothing -> pure nil
          [Atom _, _] -> throwError
                       $ TypeError "lookup: second argument must be map"
          [_, SortedMap _] -> throwError
                            $ TypeError "lookup: first argument must be atom"
          xs -> throwError $ WrongNumberOfArgs "lookup" 2 (length xs))
    -- The semantics of + and - in Scheme is a little messed up. (+ 3)
    -- evaluates to 3, and of (- 3) to -3. That's pretty intuitive.
    -- But while (+ 3 2 1) evaluates to 6, (- 3 2 1) evaluates to 0. So with -
    -- it is *not* correct to say that it's a foldl (-) 0. Instead, it
    -- special-cases on one-argument application. (Similarly with * and /.)
    --
    -- In order to avoid this sort of thing, we don't allow +,*,- and / to be
    -- applied to a single argument.
    , numBinop (+) "+"
    , numBinop (*) "*"
    , numBinop (-) "-"
    , ("<", evalArgs $ \args -> case args of
          [Number x, Number y] -> pure $ Boolean (x < y)
          [_, _] -> throwError $ TypeError "<: expecting number"
          xs -> throwError $ WrongNumberOfArgs "<" 2 (length xs))
    , (">", evalArgs $ \args -> case args of
          [Number x, Number y] -> pure $ Boolean (x > y)
          [_, _] -> throwError $ TypeError ">: expecting number"
          xs -> throwError $ WrongNumberOfArgs ">" 2 (length xs))
    , ("foldl", evalArgs $ \args -> case args of
          [fn, init', List ls] -> eval $ foldl' (\b a -> (fn $$) [b,a]) init' ls
          [_, _, _] -> throwError $ TypeError "foldl: third argument should be a list"
          xs -> throwError $ WrongNumberOfArgs "foldl" 3 (length xs))
    , ("foldr", evalArgs $ \args -> case args of
          [fn, init', List ls] -> eval $ foldr (\b a -> (fn $$) [b,a]) init' ls
          [_, _, _] -> throwError $ TypeError "foldr: third argument should be a list"
          xs -> throwError $ WrongNumberOfArgs "foldr" 3 (length xs))
    , ("map", \args -> case args of
          [fn, List ls] -> List <$> traverse eval [fn $$ [l] | l <- ls]
          [_, _, _] -> throwError $ TypeError "foldr: third argument should be a list"
          xs -> throwError $ WrongNumberOfArgs "foldr" 3 (length xs))
    , ("string?", evalArgs $ \args -> case args of
          [String _] -> pure $ Boolean True
          [_]        -> pure $ Boolean False
          xs         -> throwError $ WrongNumberOfArgs "string?" 1 (length xs))
    , ("boolean?", evalArgs $ \args -> case args of
          [Boolean _] -> pure $ Boolean True
          [_]         -> pure $ Boolean False
          xs          -> throwError $ WrongNumberOfArgs "boolean?" 1 (length xs))
    , ("number?", evalArgs $ \args -> case args of
          [Number _] -> pure $ Boolean True
          [_]        -> pure $ Boolean False
          xs         -> throwError $ WrongNumberOfArgs "number?" 1 (length xs))
    , ("member?", evalArgs $ \args -> case args of
          [x, List xs] -> fmap Boolean $ elem <$> eval x <*> traverse eval xs
          [_, _]       -> throwError
                        $ TypeError "member?: second argument must be list"
          xs           -> throwError $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("if", \args -> case args of
          [cond, t, f] -> do
            b <- eval cond
            -- I hate this as much as everyone that might ever read Haskell, but
            -- in Lisps a lot of things that one might object to are True...
            if b == Boolean False then eval f else eval t
          xs -> throwError $ WrongNumberOfArgs "if" 3 (length xs))
    , ("deref-all", \args -> do
          let derefOne :: forall b. (Data b) => (b -> Lang m b)
              derefOne x = case (cast x, eqT :: Maybe (b :~: Value)) of
                  (Just (Ref i), Just Refl) -> lookupAtom i
                  _                         -> pure x
          case args of
              [x] -> eval x >>= everywhereM derefOne
              xs  -> throwError $ WrongNumberOfArgs "deref-all" 1 (length xs))
    ]
  where
    -- Many primops evaluate their arguments just as normal functions do.
    evalArgs :: ([Value] -> Lang m Value) -> ([Value] -> Lang m Value)
    evalArgs f args = traverse eval args >>= f

    numBinop :: (Scientific -> Scientific -> Scientific)
             -> Text
             -> (Text, [Value] -> Lang m Value)
    numBinop fn name = (name, evalArgs $ \args -> case args of
        Number x:x':xs -> foldM go (Number x) (x':xs)
          where
            go (Number a) (Number b) = pure . Number $ fn a b
            go _ _ = throwError . TypeError
                   $ name <> ": expecting number"
        [Number _] -> throwError
                    $ OtherError $ name <> ": expects at least 2 arguments"
        _ -> throwError $ TypeError $ name <> ": expecting number")

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
    Number n -> pure $ Number n
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
