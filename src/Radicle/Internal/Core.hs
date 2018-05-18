{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Radicle.Internal.Core where

import           Control.Lens (At(..), Index, IxValue, Ixed(..), makeFields,
                               makePrisms, (%~), (&), (<&>), (^.), (.=))
import           Control.Monad.Except (ExceptT(..), MonadError, runExceptT,
                                       throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State
import           Data.Bifunctor (first)
import           Data.Data (Data)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Semigroup (Semigroup, (<>))
import           Data.Text (Text)
import qualified Data.Text as T
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
      Atom Ident
    | String Text
    | Boolean Bool
    | List [Value]
    | Primop Ident
    | Apply Value [Value]
    | SortedMap (Map.Map Ident Value)
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
identFromString :: String -> Ident
identFromString = Ident . fromString

-- | The environment, which keeps all known bindings.
newtype Env = Env { fromEnv :: Map Ident Value }
    deriving (Eq, Show, Ord, Read, Semigroup, Monoid, Generic, Data)

instance IsList Env where
    type Item Env = (Ident, Value)
    fromList = Env . fromList
    toList = GHC.Exts.toList . fromEnv

type instance Index Env = Ident
type instance IxValue Env = Value

instance Ixed Env
instance At Env where
  at k f (Env m) = f mv <&> \r -> Env $ case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m

-- Primop mappings
type Prims m = Map Ident ([Value] -> LangT (Bindings m) m Value)

-- | Bindings, either from the env or from the primops.
data Bindings m = Bindings
    { _bindingsEnv     :: Env
    , _bindingsPrimops :: Prims m
    } deriving (Generic)

-- | The environment in which expressions are evaluated.
newtype LangT r m a = LangT
    { fromLangM :: ExceptT LangError (StateT r m) a }
    deriving ( Functor, Applicative, Monad, MonadError LangError, MonadState r)

instance MonadTrans (LangT r) where lift = LangT . lift . lift

type Lang m = LangT (Bindings m) m

runLangM :: Bindings Identity -> Lang Identity a -> Either LangError a
runLangM e l = runIdentity $ runLangT e l

runLangT :: Monad m => Bindings m -> Lang m a -> m (Either LangError a)
runLangT e l = evalStateT (runExceptT $ fromLangM l) e

withEnv :: Monad m => (Bindings m -> Bindings m) -> Lang m a -> Lang m a
withEnv f a = do
    x <- get
    modify f
    r <- a
    put x
    pure r

-- * Lenses

makeFields ''Bindings
makePrisms ''Value
makePrisms ''LangError


-- * Functions

-- | A Bindings with an empty Env and only pure primops.
pureEmptyEnv :: Monad m => Bindings m
pureEmptyEnv = Bindings mempty purePrimops

-- | Lookup an atom in the environment
lookupAtom :: Monad m => Ident -> Lang m Value
lookupAtom i = get >>= \e -> case e ^. env . at i of
    Nothing -> throwError $ UnknownIdentifier i
    Just v  -> pure v

-- | Lookup a primop.
lookupPrimop :: Monad m => Ident -> Lang m ([Value] -> Lang m Value)
lookupPrimop i = get >>= \e -> case e ^. primops . at i of
    Nothing -> throwError $ Impossible "Unknown primop"
    Just v  -> pure v

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable via 'define'.
purePrimops :: Monad m => Prims m
purePrimops = Map.fromList $ first identFromString <$>
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
              env . at name .= Just val'
              pure $ List [Atom $ identFromString "set!", Atom name, val']
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
eval :: Monad m => Value -> Lang m Value
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
                  withEnv (\e -> e & env %~ modEnv) (eval body)
            _ -> throwError $ TypeError "Trying to apply a non-function"
    Primop i -> pure $ Primop i
    e@(Lambda _ _ (Just _)) -> pure e
    Lambda args body Nothing -> gets $ \e -> Lambda args body (Just $ e ^. env)
    SortedMap mp -> do
        let evalSnd (a,b) = (a ,) <$> eval b
        SortedMap . Map.fromList <$> traverse evalSnd (Map.toList mp)

-- * Helpers

-- | Infix function application
infixr 1 $$
($$) :: Value -> [Value] -> Value
($$) = Apply

nil :: Value
nil = List []
