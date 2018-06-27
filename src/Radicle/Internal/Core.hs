{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Radicle.Internal.Core where

import           Control.Monad.Except (ExceptT(..), MonadError, runExceptT,
                                       throwError)
import           Control.Monad.ST.Trans (STRef, STT, newSTRef, readSTRef,
                                         runSTT, writeSTRef)
import           Control.Monad.State
import           Data.Bifunctor (first)
import           Data.Data (Data)
import           Data.Deriving (deriveEq1, deriveShow1)
import           Data.Functor.Foldable (Fix(..), cata)
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.Scientific (Scientific)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Text (Text)
import           Data.Void (Void)
import           GHC.Exts (IsList(..), fromString)
import           GHC.Generics (Generic)
import qualified Text.Megaparsec.Error as Par
import           Unsafe.Coerce (unsafeCoerce)

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

newtype Reference s = Reference { getReference :: STRef s (Value (Reference s)) }
    deriving (Eq)

-- | Create a new ref with the supplied initial value.
newRef :: Monad m => Value (Reference s) -> Lang s m (Value (Reference s))
newRef v = LangT $ lift $ lift $ Ref . Reference <$> newSTRef v

-- | Read the value of a reference.
deref :: Monad m => Reference s -> Lang s m (Value (Reference s))
deref (Reference r) = LangT . lift . lift $ readSTRef r

-- | An expression or value in the language.
--
-- The parameter is for refs. Usually it is 'Reference s' (with 's' being the
-- thread param of the ST monad). However, we first parse into 'Fix Value'
-- before converting it into 'Reference' (see 'makeRefs').
--
-- A Value that no longer contains any references can have type 'Value Void',
-- indicating it is safe to share between threads or chains.
data Value r =
    -- | A regular (hyperstatic) variable.
      Atom Ident
    | String Text
    | Number Scientific
    | Boolean Bool
    | List [Value r]
    | Primop Ident
    | Apply (Value r) [Value r]
    | SortedMap (Map.Map Ident (Value r))
    | Ref r
    -- | Takes the arguments/parameters, a body, and possibly a closure.
    --
    -- The value of an application of a lambda is always the last value in the
    -- body. The only reason to have multiple values is thus only for (local)
    -- "define"s.
    | Lambda [Ident] (NonEmpty (Value r)) (Maybe (Env (Value r)))
    deriving (Eq, Show, Read, Generic, Functor, Foldable, Traversable)


-- | Replace all Refs containing 'Fix Value' into ones containing references to
-- those values.
makeRefs :: Monad m => Value (Fix Value) -> Lang s m (Value (Reference s))
makeRefs v = cata go (Fix v)
  where
    go x = case x of
        Atom i -> pure $ Atom i
        String i -> pure $ String i
        Boolean i -> pure $ Boolean i
        Number i -> pure $ Number i
        Primop i -> pure $ Primop i
        Apply f vs -> Apply <$> go f <*> sequence (go <$> vs)
        SortedMap m -> SortedMap <$> sequence (go <$> m)
        List vs -> List <$> sequence (go <$> vs)
        Ref i -> i >>= newRef
        Lambda is bd e -> Lambda is <$> sequence (go <$> bd)
                                    <*> traverse sequence (fmap go <$> e)

-- | Replace all refs containing a 'Reference' into ones containing a number
-- representing that 'Reference'.
labelRefs :: Value (Reference s) -> Value Int
labelRefs v = evalState (traverse go v) (0, [])
  where
    go :: Reference s -> State (Int, [(Reference s, Int)]) Int
    go s = get >>= \(cur, mapping) -> case lookup s mapping of
        Nothing -> put (cur + 1, (s, cur):mapping) >> pure cur
        Just ix -> pure ix

-- | Safely coerce a 'Value' containing no refs into one of a different type.
coerceRefs :: Value Void -> Value a
coerceRefs = unsafeCoerce

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
newtype Env s = Env { fromEnv :: Map Ident s }
    deriving (Eq, Semigroup, Monoid, Show, Read, Generic, Functor, Foldable, Traversable)

instance IsList (Env s) where
    type Item (Env s) = (Ident, s)
    fromList = Env . fromList
    toList = GHC.Exts.toList . fromEnv

-- | Primop mappings. The parameter specifies the monad the primops run in.
type Primops s m = Map Ident ([Value (Reference s)] -> Lang s m (Value (Reference s)))

-- | Bindings, either from the env or from the primops.
data Bindings s m = Bindings
    { bindingsEnv     :: Env (Value (Reference s))
    , bindingsPrimops :: Primops s m
    } deriving (Generic)

-- | The environment in which expressions are evaluated.
newtype LangT s r m a = LangT
    { fromLangT :: ExceptT LangError (StateT r (STT s m)) a }
    deriving (Functor, Applicative, Monad, MonadError LangError, MonadState r)

instance MonadTrans (LangT s r) where lift = LangT . lift . lift . lift

-- | A monad for language operations specialized to have as state the Bindings
-- with appropriate underlying monad.
type Lang s m = LangT s (Bindings s m) m

runLang
    :: Monad m
    => (forall s. Bindings s m)
    -> (forall s. Lang s m a)
    -> m (Either LangError a)
runLang e l = runSTT $ evalStateT (runExceptT $ fromLangT l) e

-- | Like 'local' or 'withState'
withEnv :: Monad m => (Bindings s m -> Bindings s m) -> Lang s m a -> Lang s m a
withEnv modifier action = do
    oldEnv <- get
    modify modifier
    res <- action
    put oldEnv
    pure res

-- * Functions

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: (Monad m) => Bindings r m
pureEnv = Bindings e purePrimops
  where
    e = fromList [(toIdent "eval", Primop $ toIdent "base-eval")]

addBinding :: Monad m => Ident -> Value (Reference r) -> Bindings r m -> Bindings r m
addBinding i v b = b
    { bindingsEnv = Env . Map.insert i v . fromEnv $ bindingsEnv b }

-- | Lookup an atom in the environment
lookupAtom :: Monad m => Ident -> Lang r m (Value (Reference r))
lookupAtom i = get >>= \e -> case Map.lookup i . fromEnv $ bindingsEnv e of
    Nothing -> throwError $ UnknownIdentifier i
    Just v  -> pure v

-- | Lookup a primop.
lookupPrimop :: Monad m => Ident -> Lang r m ([Value (Reference r)] -> Lang r m (Value (Reference r)))
lookupPrimop i = get >>= \e -> case Map.lookup i $ bindingsPrimops e of
    Nothing -> throwError $ Impossible "Unknown primop"
    Just v  -> pure v

defineAtom :: Monad m => Ident -> Value (Reference r) -> Lang r m ()
defineAtom i v = modify $ addBinding i v

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable via 'define'.
purePrimops :: forall r m. (Monad m) => Primops r m
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
          [List (x:_)] -> pure x
          [List []]    -> throwError $ OtherError "car: empty list"
          [_]          -> throwError $ TypeError "car: expects list argument"
          xs           -> throwError $ WrongNumberOfArgs "car" 1 (length xs))
    , ("cdr", evalArgs $ \args -> case args of
          [List (_:xs)] -> pure $ List xs
          [List []]     -> throwError $ OtherError "cdr: empty list"
          [_]           -> throwError $ TypeError "cdr: expects list argument"
          xs            -> throwError $ WrongNumberOfArgs "cdr" 1 (length xs))
    , ("lookup", evalArgs $ \args -> case args of
          [Atom a, SortedMap m] -> case Map.lookup a m of
              Just v  -> eval v
              -- Probably an exception is better, but that seems cruel
              -- when you have no exception handling facilities.
              Nothing -> pure nil
          [Atom _, _] -> throwError
                       $ TypeError "lookup: second argument must be map"
          [_, SortedMap _] -> throwError
                            $ TypeError "lookup: first argument must be atom"
          xs -> throwError $ WrongNumberOfArgs "lookup" 2 (length xs))
    , ("string-append", evalArgs $ \args ->
          let fromStr (String s) = Just s
              fromStr _          = Nothing
              ss = fromStr <$> args
          in if all isJust ss
              then pure . String . mconcat $ catMaybes ss
              else throwError $ TypeError "string-append: non-string argument")
    , ("insert", evalArgs $ \args -> case args of
          [Atom k, v, SortedMap m] -> pure . SortedMap $ Map.insert k v m
          [Atom _, _, _] -> throwError
                          $ TypeError "insert: third argument must be map"
          [_, _, _] -> throwError
                     $ TypeError "insert: first argument must be an atom"
          xs -> throwError $ WrongNumberOfArgs "insert" 3 (length xs))
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
          [_, _] -> throwError $ TypeError "map: second argument should be a list"
          xs -> throwError $ WrongNumberOfArgs "map" 3 (length xs))
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
    , ("deref", evalArgs $ \args -> do
          case args of
              [Ref x] -> eval =<< deref x
              [_] -> throwError $ TypeError "deref: argument must be a ref"
              xs  -> throwError $ WrongNumberOfArgs "deref" 1 (length xs))
    , ("write-ref", evalArgs $ \args -> do
          case args of
              [Ref (Reference x), v] -> LangT (lift $ lift $ writeSTRef x v) >> pure nil
              [_, _] -> throwError $ TypeError "write-ref: first argument must be a ref"
              xs  -> throwError $ WrongNumberOfArgs "write-ref" 2 (length xs))
    ]
  where
    -- Many primops evaluate their arguments just as normal functions do.
    evalArgs f args = traverse eval args >>= f

    numBinop :: (Scientific -> Scientific -> Scientific)
             -> Text
             -> (Text, [Value (Reference r)] -> Lang r m (Value (Reference r)))
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
eval :: Monad m => Value (Reference r) -> Lang r m (Value (Reference r))
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
              NonEmpty.last <$> withEnv (\e' -> e' { bindingsEnv = modEnv})
                                        (traverse eval body)
        _ -> throwError $ TypeError "Trying to apply a non-function"

-- | The built-in, original, eval.
baseEval :: Monad m => Value (Reference r) -> Lang r m (Value (Reference r))
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
            -- This happens if a quoted lambda is explicitly evaled. We then
            -- give it the current environment.
            Lambda bnds body Nothing -> do
                  vs' <- traverse baseEval vs
                  let mappings = fromList (zip bnds vs')
                  NonEmpty.last <$> withEnv
                      (\e -> e { bindingsEnv = mappings <> bindingsEnv e })
                      (traverse baseEval body)
            Lambda bnds body (Just closure) -> do
                  vs' <- traverse baseEval vs
                  let mappings = fromList (zip bnds vs')
                      modEnv = mappings <> closure
                  NonEmpty.last <$> withEnv (\e -> e { bindingsEnv = modEnv })
                                            (traverse baseEval body)
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
($$) :: Value r -> [Value r] -> Value r
($$) = Apply

nil :: Value r
nil = List []

-- TH
deriveEq1 ''Env
deriveEq1 ''Value
deriveShow1 ''Env
deriveShow1 ''Value
