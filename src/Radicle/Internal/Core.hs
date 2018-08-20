module Radicle.Internal.Core where

import           Protolude hiding (TypeError, (<>))

import           Control.Monad.State
import           Data.Data (Data)
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Data.Semigroup ((<>))
import qualified GHC.Exts as GhcExts
import qualified Text.Megaparsec.Error as Par

-- * Value

-- | An error throw during parsing or evaluating expressions in the language.
data LangError r =
      UnknownIdentifier Ident
    | Impossible Text
    | TypeError Text
    -- | Takes the function name, expected number of args, and actual number of
    -- args
    | WrongNumberOfArgs Text Int Int
    | OtherError Text
    | ParseError (Par.ParseError Char Void)
    | ThrownError Ident r
    | Exit
    deriving (Eq, Show, Read, Generic, Functor)

-- | Convert an error to a radicle value, and the label for it. Used for
-- catching exceptions.
errorToValue
    :: Monad m
    => LangError Value
    -> Lang m (Ident, Value)
errorToValue e = case e of
    UnknownIdentifier i -> makeVal
        ( "unknown-identifier"
        , [("identifier", makeA i)]
        )
    -- "Now more than ever seems it rich to die"
    Impossible _ -> throwError e
    TypeError i -> makeVal
        ( "type-error"
        , [("info", String i)]
        )
    WrongNumberOfArgs i expected actual -> makeVal
        ( "wrong-number-of-args"
        , [ ("function", makeA $ Ident i)
          , ("expected", Number $ fromIntegral expected)
          , ("actual", Number $ fromIntegral actual)]
        )
    OtherError i -> makeVal
        ( "other-error"
        , [("info", String i)]
        )
    ParseError _ -> makeVal ("parse-error", [])
    ThrownError label val -> pure (label, val)
    Exit -> makeVal ("exit", [])
  where
    makeA = quote . Atom
    makeVal (t,v) = pure (Ident t, Dict $ Map.mapKeys (Atom . Ident) . GhcExts.fromList $ v)

newtype Reference = Reference { getReference :: Int }
    deriving (Show, Read, Ord, Eq, Generic)

-- | Create a new ref with the supplied initial value.
newRef :: Monad m => Value -> Lang m Value
newRef v = do
    b <- get
    let ix = bindingsNextRef b
    put $ b { bindingsNextRef = succ ix
            , bindingsRefs = IntMap.insert ix v $ bindingsRefs b
            }
    pure . Ref $ Reference ix

-- | Read the value of a reference.
readRef :: MonadError (LangError Value) m => Reference -> Lang m Value
readRef (Reference r) = do
    refs <- gets bindingsRefs
    case IntMap.lookup r refs of
        Nothing -> throwError $ Impossible "undefined reference"
        Just v  -> pure v

-- | An expression or value in the language.
data Value =
    -- | A regular (hyperstatic) variable.
      Atom Ident
    -- | Symbolic identifiers that evaluate to themselves.
    | Keyword Ident
    | String Text
    | Number Scientific
    | Boolean Bool
    | List [Value]
    | Primop Ident
    | Dict (Map.Map Value Value)
    | Ref Reference
    -- | Takes the arguments/parameters, a body, and possibly a closure.
    --
    -- The value of an application of a lambda is always the last value in the
    -- body. The only reason to have multiple values is for effects.
    | Lambda [Ident] (NonEmpty Value) (Maybe (Env Value))
    deriving (Eq, Show, Ord, Read, Generic)


-- | An identifier in the language.
--
-- Not all `Text`s are valid identifiers, so we do not export the constructor.
-- Instead, use `makeIdent`.
newtype Ident = Ident { fromIdent :: Text }
    deriving (Eq, Show, Read, Ord, Generic, Data)

-- Unsafe! Only use this if you know the string at compile-time and know it's a
-- valid identifier
toIdent :: Text -> Ident
toIdent = Ident

-- | The environment, which keeps all known bindings.
newtype Env s = Env { fromEnv :: Map Ident s }
    deriving (Eq, Semigroup, Monoid, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

instance GhcExts.IsList (Env s) where
    type Item (Env s) = (Ident, s)
    fromList = Env . Map.fromList
    toList = GhcExts.toList . fromEnv

-- | Primop mappings. The parameter specifies the monad the primops run in.
type Primops m = Map Ident ([Value ] -> Lang m Value)

-- | Bindings, either from the env or from the primops.
data Bindings m = Bindings
    { bindingsEnv     :: Env Value
    , bindingsPrimops :: Primops m
    , bindingsRefs    :: IntMap Value
    , bindingsNextRef :: Int
    } deriving (Generic)

-- | The environment in which expressions are evaluated.
newtype LangT r m a = LangT
    { fromLangT :: ExceptT (LangError Value) (StateT r m) a }
    deriving (Functor, Applicative, Monad, MonadError (LangError Value), MonadState r)

instance MonadTrans (LangT r) where lift = LangT . lift . lift

-- | A monad for language operations specialized to have as state the Bindings
-- with appropriate underlying monad.
type Lang m = LangT (Bindings m) m

runLang
    :: Bindings m
    -> Lang m a
    -> m (Either (LangError Value) a, Bindings m)
runLang e l = runStateT (runExceptT $ fromLangT l) e

-- | Like 'local' or 'withState'. Will run an action with a modified environment
-- and then restore the original environment. Other bindings (i.e. primops and
-- refs) are not affected.
withEnv :: Monad m => (Env Value -> Env Value) -> Lang m a -> Lang m a
withEnv modifier action = do
    oldEnv <- gets bindingsEnv
    modify $ \s -> s { bindingsEnv = modifier oldEnv }
    res <- action
    modify $ \s -> s { bindingsEnv = oldEnv }
    pure res

-- * Functions

addBinding :: Ident -> Value -> Bindings m -> Bindings m
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

quote :: Value -> Value
quote v = List [Primop (Ident "quote"), v]

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
            fn [quote val]
        Lambda _ _ Nothing -> throwError $ Impossible
            "lambda should already have an env"
        Lambda [bnd] body (Just closure) -> do
              let mappings = GhcExts.fromList [(bnd, val)]
                  modEnv = mappings <> closure
              NonEmpty.last <$> withEnv (const modEnv)
                                        (traverse eval body)
        _ -> throwError $ TypeError "Trying to apply a non-function"

-- | The built-in, original, eval.
baseEval :: Monad m => Value -> Lang m Value
baseEval val = case val of
    Atom i -> lookupAtom i
    kw@(Keyword _) -> pure kw
    Ref i -> pure $ Ref i
    List (f:vs) -> f $$ vs
    List xs -> throwError
        $ WrongNumberOfArgs ("application: " <> show xs)
                            2
                            (length xs)
    String s -> pure $ String s
    Number n -> pure $ Number n
    Boolean b -> pure $ Boolean b
    Primop i -> pure $ Primop i
    e@(Lambda _ _ (Just _)) -> pure e
    Lambda args body Nothing -> gets $ Lambda args body . Just . bindingsEnv
    Dict mp -> do
        let evalSnd (a,b) = (a ,) <$> baseEval b
        Dict . Map.fromList <$> traverse evalSnd (Map.toList mp)



-- * Helpers

-- | Infix function application
infixr 1 $$
($$) :: Monad m => Value -> [Value] -> Lang m Value
mfn $$ vs = do
    mfn' <- baseEval mfn
    case mfn' of
        Primop i -> do
            fn <- lookupPrimop i
            -- Primops get to decide whether and how their args are
            -- evaluated.
            fn vs
        -- This happens if a quoted lambda is explicitly evaled. We then
        -- give it the current environment.
        Lambda bnds body Nothing ->
            if length bnds /= length vs
                then throwError $ WrongNumberOfArgs "lambda" (length bnds)
                                                             (length vs)
                else do
                    vs' <- traverse baseEval vs
                    let mappings = GhcExts.fromList (zip bnds vs')
                    NonEmpty.last <$> withEnv
                        (mappings <>)
                        (traverse baseEval body)
        Lambda bnds body (Just closure) ->
            if length bnds /= length vs
                then throwError $ WrongNumberOfArgs "lambda" (length bnds)
                                                             (length vs)
                else do
                    vs' <- traverse baseEval vs
                    let mappings = GhcExts.fromList (zip bnds vs')
                        modEnv = mappings <> closure
                    NonEmpty.last <$> withEnv (const modEnv)
                                              (traverse baseEval body)
        _ -> throwError $ TypeError "Trying to apply a non-function"

nil :: Value
nil = List []
