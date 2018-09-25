-- | The core radicle datatypes and functionality.
module Radicle.Internal.Core where

import           Protolude hiding (TypeError, (<>), list)

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
                 (ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.State
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as A
import           Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Data.Semigroup ((<>))
import qualified GHC.Exts as GhcExts
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Orphans ()

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
    makeVal (t,v) = pure (Ident t, Dict $ Map.mapKeys (Keyword . Ident) . GhcExts.fromList $ v)

newtype Reference = Reference { getReference :: Int }
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

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
readRef :: Monad m => Reference -> Lang m Value
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
    | Lambda [Ident] (NonEmpty Value) (Env Value)
    deriving (Eq, Show, Ord, Read, Generic)

instance Serialise Value

-- Should just be a prism
isAtom :: Value -> Maybe Ident
isAtom (Atom i) = pure i
isAtom _        = Nothing

instance A.FromJSON Value where
  parseJSON = \case
    A.Number n -> pure $ Number n
    A.String s -> pure $ String s
    A.Array ls -> List . toList <$> traverse parseJSON ls
    A.Bool b -> pure $ Boolean b
    A.Null -> pure $ Keyword (toIdent "null")
    A.Object hm -> do
      let kvs = HashMap.toList hm
      vs <- traverse parseJSON (snd <$> kvs)
      pure . Dict . Map.fromList $ zip (String . fst <$> kvs) vs

-- | Convert a radicle `Value` into an 'aeson' value, if possible.
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ List [Number 3, String "hi"]
-- "[3,\"hi\"]"
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(String "foo", String "bar")]
-- "{\"foo\":\"bar\"}"
--
-- This fails for lambdas, since lambdas capture an entire environment
-- (possibly recursively). It also fails for dictionaries with non-string key
-- non-string keys.
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(Number 3, String "bar")]
-- "null"
maybeJson :: Value -> Maybe A.Value
maybeJson = \case
    Number n -> pure $ A.Number n
    String s -> pure $ A.String s
    Boolean b -> pure $ A.Bool b
    List ls -> toJSON <$> traverse maybeJson ls
    Dict m -> do
      let kvs = Map.toList m
      ks <- traverse isStr (fst <$> kvs)
      vs <- traverse maybeJson (snd <$> kvs)
      pure $ A.Object (HashMap.fromList (zip ks vs))
    _ -> Nothing
  where
    isStr (String s) = pure s
    isStr _          = Nothing

-- | An identifier in the language.
--
-- Not all `Text`s are valid identifiers, so use 'Ident' at your own risk.
-- `mkIdent` is the safe version.
newtype Ident = Ident { fromIdent :: Text }
    deriving (Eq, Show, Read, Ord, Generic, Data, Serialise)

-- Unsafe! Only use this if you know the string at compile-time and know it's a
-- valid identifier
toIdent :: Text -> Ident
toIdent = Ident

-- | The environment, which keeps all known bindings.
newtype Env s = Env { fromEnv :: Map Ident s }
    deriving (Eq, Semigroup, Monoid, Ord, Show, Read, Generic, Functor, Foldable, Traversable, Serialise)

instance GhcExts.IsList (Env s) where
    type Item (Env s) = (Ident, s)
    fromList = Env . Map.fromList
    toList = GhcExts.toList . fromEnv

-- | Primop mappings. The parameter specifies the monad the primops run in.
newtype Primops m = Primops { getPrimops :: Map Ident ([Value ] -> Lang m Value) }
  deriving (Semigroup)

-- | Bindings, either from the env or from the primops.
data Bindings prims = Bindings
    { bindingsEnv     :: Env Value
    , bindingsPrimops :: prims
    , bindingsRefs    :: IntMap Value
    , bindingsNextRef :: Int
    } deriving (Eq, Show, Functor, Generic)

-- | The environment in which expressions are evaluated.
newtype LangT r m a = LangT
    { fromLangT :: ExceptT (LangError Value) (StateT r m) a }
    deriving (Functor, Applicative, Monad, MonadError (LangError Value), MonadIO, MonadState r)

instance MonadTrans (LangT r) where lift = LangT . lift . lift

-- | A monad for language operations specialized to have as state the Bindings
-- with appropriate underlying monad.
type Lang m = LangT (Bindings (Primops m)) m

-- | Run a `Lang` computation with the provided bindings. Returns the result as
-- well as the updated bindings.
runLang
    :: Bindings (Primops m)
    -> Lang m a
    -> m (Either (LangError Value) a, Bindings (Primops m))
runLang e l = runStateT (runExceptT $ fromLangT l) e

-- | Like 'local' or 'withState'. Will run an action with a modified environment
-- and then restore the original bindings.
withBindings :: Monad m => (Bindings (Primops m) -> Bindings (Primops m)) -> Lang m a -> Lang m a
withBindings modifier action = do
    oldBnds <- get
    modify modifier
    res <- action
    put oldBnds
    pure res

-- | Like 'local' or 'withState'. Will run an action with a modified environment
-- and then restore the original environment. Other bindings (i.e. primops and
-- refs) are not affected.
withEnv :: Monad m => (Env Value -> Env Value) -> Lang m a -> Lang m a
withEnv modifier action =
  let mod s = s { bindingsEnv = modifier $ bindingsEnv s}
  in withBindings mod action

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
lookupPrimop i = get >>= \e -> case Map.lookup i $ getPrimops $ bindingsPrimops e of
    Nothing -> throwError $ Impossible "Unknown primop"
    Just v  -> pure v

defineAtom :: Monad m => Ident -> Value -> Lang m ()
defineAtom i v = modify $ addBinding i v

-- * Eval

-- | The buck-passing eval. Uses whatever 'eval' is in scope.
eval :: Monad m => Value -> Lang m Value
eval val = do
    e <- lookupAtom (toIdent "eval")
    st <- gets toRadicle
    case e of
        Primop i -> do
            fn <- lookupPrimop i
            -- Primops get to decide whether and how their args are
            -- evaluated.
            res <- fn [quote val, quote st]
            updateEnvAndReturn res
        Lambda [exprBnd, envBnd] body (Just closure) -> do
              let mappings = GhcExts.fromList [(exprBnd, val), (envBnd, st)]
                  modEnv = mappings <> closure
              res <- NonEmpty.last <$> withEnv (const modEnv) (traverse eval body)
              updateEnvAndReturn res
        _ -> throwError $ TypeError "Trying to apply a non-function"
  where
    updateEnvAndReturn :: Monad m => Value -> Lang m Value
    updateEnvAndReturn v = case v of
        List [val', newEnv] -> do
            newEnv' <- either (throwError . OtherError) pure $ fromRadicle newEnv
            modify $ \s -> s { bindingsEnv = newEnv' }
            return val'
        _ -> throwError $ OtherError "eval: should return list with value and new env"



-- | The built-in, original, eval.
baseEval :: Monad m => Value -> Lang m Value
baseEval val = case val of
    Atom i -> lookupAtom i
    List (f:vs) -> f $$ vs
    List xs -> throwError
        $ WrongNumberOfArgs ("application: " <> show xs)
                            2
                            (length xs)
    Dict mp -> do
        let evalBoth (a,b) = (,) <$> baseEval a <*> baseEval b
        Dict . Map.fromList <$> traverse evalBoth (Map.toList mp)
    autoquote -> pure autoquote

-- * From/ToRadicle

class FromRadicle a where
    fromRadicle :: Value -> Either Text a

instance FromRadicle Scientific where
    fromRadicle x = case x of
        Number n -> pure n
        _ -> Left "Expecting number"
instance FromRadicle Text where
    fromRadicle x = case x of
        String n -> pure n
        _ -> Left "Expecting string"
instance FromRadicle a => FromRadicle [a] where
    fromRadicle x = case x of
        List xs -> traverse fromRadicle xs
        _ -> Left "Expecting list"
instance FromRadicle (Env Value) where
    fromRadicle x = case x of
        Dict d -> fmap (Env . Map.fromList)
                $ forM (Map.toList d) $ \(k, v) -> case k of
            Atom i -> pure (i, v)
            k'     -> Left $ "Expecting atom keys. Got: " <> show k'
        _ -> Left "Expecting dict"
instance FromRadicle (Bindings ()) where
    fromRadicle x = case x of
        Dict d -> do
            env' <- kwLookup "env" d ?? "Expecting 'env' key"
            refs' <- kwLookup "refs" d ?? "Expecting 'refs' key"
            (nextRef, refs) <- makeRefs refs'
            env <- fromRadicle env'
            pure $ Bindings env () refs nextRef
        _ -> throwError "Expecting dict"
      where
        makeRefs refs = case refs of
            List ls -> pure (length ls, IntMap.fromList $ zip [0..] ls)
            _       -> throwError $ "Expecting dict"



class ToRadicle a where
    toRadicle :: a -> Value

instance ToRadicle Int where
    toRadicle = Number . fromIntegral
instance ToRadicle Scientific where
    toRadicle = Number
instance ToRadicle Text where
    toRadicle = String
instance ToRadicle a => ToRadicle [a] where
    toRadicle xs = List $ toRadicle <$> xs
instance ToRadicle a => ToRadicle (Map.Map Text a) where
    toRadicle xs = Dict $ Map.mapKeys String $ toRadicle <$> xs
instance ToRadicle (Env Value) where
    toRadicle x = Dict . Map.mapKeys Atom $ fromEnv x
instance ToRadicle (Bindings m) where
    toRadicle x = Dict $ Map.fromList
        [ (Keyword $ Ident "env", toRadicle $ bindingsEnv x)
        , (Keyword $ Ident "refs", List $ IntMap.elems (bindingsRefs x))
        ]

-- * Helpers


callFn :: Monad m => Value -> [Value] -> Lang m Value
callFn f vs = case f of
  Lambda bnds body closure ->
      if length bnds /= length vs
          then throwError $ WrongNumberOfArgs "lambda" (length bnds)
                                                       (length vs)
          else do
              let mappings = GhcExts.fromList (zip bnds vs)
                  modEnv = mappings <> closure
              NonEmpty.last <$> withEnv (const modEnv)
                                        (traverse baseEval body)
  Primop i -> throwError . TypeError
    $ "Trying to call a non-function: the primop '" <> show i
    <> "' cannot be used as a function."
  _ -> throwError . TypeError $ "Trying to call a non-function."

-- | Infix evaluation of application (of functions or primops)
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
        f@Lambda{} -> do
          vs' <- traverse baseEval vs
          callFn f vs'
        _ -> throwError $ TypeError "Trying to apply a non-function"


nil :: Value
nil = List []

quote :: Value -> Value
quote v = List [Primop (Ident "quote"), v]

list :: [Value] -> Value
list vs = List (Primop (Ident "list") : vs)

kwLookup :: Text -> Map Value Value -> Maybe Value
kwLookup key = Map.lookup (Keyword $ Ident key)

(??) :: MonadError e m => Maybe a -> e -> m a
a ?? n = n `note` a
