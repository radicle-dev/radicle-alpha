{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- | The core radicle datatypes and functionality.
module Radicle.Internal.Core where

import qualified Prelude
import           Protolude hiding (Constructor, Handle, TypeError, (<>))

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
                 (ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.State
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as A
import           Data.Copointed (Copointed(..))
import qualified GHC.IO.Handle as Handle
import           Data.Data (Data)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Data.Semigroup ((<>))
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Generics.Eot
import qualified GHC.Exts as GhcExts
import           System.Process (CmdSpec(..), CreateProcess(..), StdStream(..), ProcessHandle)
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Annotation (Annotated)
import qualified Radicle.Internal.Annotation as Ann
import qualified Radicle.Internal.Doc as Doc
import qualified Radicle.Internal.Identifier as Identifier
import qualified Radicle.Internal.Number as Num
import           Radicle.Internal.Orphans ()
import           Radicle.Internal.Type (Type(..))
import qualified Radicle.Internal.Type as Type


-- * Value

data LangError r = LangError [Ann.SrcPos] (LangErrorData r)
    deriving (Eq, Show, Read, Generic, Functor)

instance Serialise r => Serialise (LangError r)

data PatternMatchError
  = NoMatch
  | NoValue
  | BadBindings Value
  deriving (Eq, Show, Read, Generic)

instance Serialise PatternMatchError

-- | An error thrown during parsing or evaluating expressions in the language.
data LangErrorData r =
      UnknownIdentifier Ident
    | Impossible Text
    -- | The special form that was misused, and information on the misuse.
    | SpecialForm Text Text
    | NonFunctionCalled Value
    -- | Takes a function description, a argument position, an expected type and
    -- the value which wasn't of that type.
    | TypeError Text Int Type.Type Value
    -- | Takes the function name, expected number of args, and actual number of
    -- args
    | WrongNumberOfArgs Text Int Int
    | NonHashableKey
    | OtherError Text
    | ParseError (Par.ParseError Char Void)
    -- | Raised if @(throw ident value)@ is evaluated. Arguments are
    -- provided by the call to @throw@.
    | ThrownError Ident r
    | PatternMatchError PatternMatchError
    -- | Raised if the effectful @exit!@ primitive is evaluated.
    | Exit
    deriving (Eq, Show, Read, Generic, Functor)

instance Serialise r => Serialise (LangErrorData r)

throwErrorHere :: (MonadError (LangError Value) m, HasCallStack) => LangErrorData Value -> m a
throwErrorHere = withFrozenCallStack (throwError . LangError [Ann.thisPos])

toLangError :: (HasCallStack) => LangErrorData Value -> LangError Value
toLangError = LangError [Ann.thisPos]

-- | Remove callstack information.
noStack :: Either (LangError Value) a -> Either (LangErrorData Value) a
noStack (Left (LangError _ err)) = Left err
noStack (Right v)                = Right v

typeToValue :: Type.Type -> Value
typeToValue = Keyword . Ident . Identifier.kebabCons . drop 1 . show

-- | Convert an error to a radicle value, and the label for it. Used for
-- catching exceptions.
errorDataToValue
    :: Monad m
    => LangErrorData Value
    -> Lang m (Ident, Value)
errorDataToValue e = case e of
    UnknownIdentifier i -> makeVal
        ( "unknown-identifier"
        , [("identifier", makeA i)]
        )
    -- "Now more than ever seems it rich to die"
    Impossible _ -> throwErrorHere e
    TypeError fname pos ty v -> makeVal
        ( "type-error"
        , [ ("function", makeA $ Ident fname)
          , ("position", Number (fromIntegral pos))
          , ("expected-type", typeToValue ty)
          , ("actual-type", typeToValue (valType v))
          , ("value", v)
          ]
        )
    NonFunctionCalled v -> makeVal
      ( "non-function-called"
      , [("value", v)])
    SpecialForm form info -> makeVal
      ( "special-form-error"
      , [ ("special-form", makeA $ Ident form)
        , ("info", String info)])
    WrongNumberOfArgs i expected actual -> makeVal
        ( "wrong-number-of-args"
        , [ ("function", makeA $ Ident i)
          , ("expected", Number $ fromIntegral
              expected)
          , ("actual", Number $ fromIntegral actual)]
        )
    NonHashableKey -> makeVal ( "non-hashable-key", [])
    OtherError i -> makeVal
        ( "other-error"
        , [("info", String i)]
        )
    ParseError _ -> makeVal ("parse-error", [])
    ThrownError label val -> pure (label, val)
    PatternMatchError pe -> case pe of
      NoValue       -> makeVal ( "no-value-to-match", [])
      NoMatch       -> makeVal ( "non-exhaustive-pattern-matches", [])
      BadBindings p -> makeVal ( "bad-pattern", [("bad-pattern", p)])
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
        Nothing -> throwErrorHere $ Impossible "undefined reference"
        Just v  -> pure v

-- | As with References and ProcHdls, we keep a counter for each handle, and in
-- the environment map those to actual handles.
newtype Hdl = Hdl { getHandle :: Int }
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

-- | Create a new handle.
newHandle :: Monad m => Handle.Handle -> Lang m Value
newHandle h = do
    b <- get
    let ix = bindingsNextHandle b
    put $ b { bindingsNextHandle = succ ix
            , bindingsHandles = IntMap.insert ix h $ bindingsHandles b
            }
    pure $ Handle $ Hdl ix

lookupHandle :: Monad m => Hdl -> Lang m Handle.Handle
lookupHandle (Hdl h) = do
    hdls <- gets bindingsHandles
    case IntMap.lookup h hdls of
        Nothing -> throwErrorHere $ Impossible "no such handle"
        Just v  -> pure v

-- | As with References and Hdls, we keep a counter for each handle, and in the
-- environment map those to actual handles.
newtype ProcHdl = ProcHdl { getProcHandle :: Int }
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

lookupProcHandle :: Monad m => ProcHdl -> Lang m ProcessHandle
lookupProcHandle (ProcHdl h) = do
    hdls <- gets bindingsProcHandles
    case IntMap.lookup h hdls of
        Nothing -> throwErrorHere $ Impossible "no such process handle"
        Just v  -> pure v

-- | Create a new process handle.
newProcessHandle :: Monad m => ProcessHandle -> Lang m Value
newProcessHandle h = do
    b <- get
    let ix = bindingsNextProcHandle b
    put $ b { bindingsNextProcHandle = succ ix
            , bindingsProcHandles = IntMap.insert ix h $ bindingsProcHandles b
            }
    pure $ ProcHandle $ ProcHdl ix

-- | An expression or value in the language.
data ValueF r =
    -- | A regular (hyperstatic) variable.
      AtomF Ident
    -- | Symbolic identifiers that evaluate to themselves.
    | KeywordF Ident
    | StringF Text
    | NumberF Rational
    | BooleanF Bool
    | ListF [r]
    | VecF (Seq r)
    | PrimFnF Ident
    -- | Map from *pure* Values -- annotations shouldn't change lookup semantics.
    | DictF (Map.Map Value r)
    | RefF Reference
    | HandleF Hdl
    | ProcHandleF ProcHdl
    -- | Takes the arguments/parameters, a body, and possibly a closure.
    --
    -- The value of an application of a lambda is always the last value in the
    -- body. The only reason to have multiple values is for effects.
    | LambdaF [Ident] (NonEmpty r) (Env r)
    deriving (Eq, Ord, Read, Show, Generic, Functor)

instance Serialise r => Serialise (ValueF r)

valType :: (CPA t) => Annotated t ValueF -> Type.Type
valType = \case
  Atom _ -> TAtom
  Keyword _ -> TKeyword
  String _ -> TString
  Number _ -> TNumber
  Boolean _ -> TBoolean
  List _ -> TList
  Vec _ -> TVec
  PrimFn _ -> TFunction
  Dict _ -> TDict
  Ref _ -> TRef
  Handle _ -> THandle
  ProcHandle _ -> TProcHandle
  Lambda{} -> TFunction

hashable :: (CPA t) => Annotated t ValueF -> Bool
hashable = \case
  Atom _ -> True
  Boolean _ -> True
  Number _ -> True
  String _ -> True
  Keyword _ -> True
  PrimFn _ -> False
  Ref _ -> False
  Handle _ -> False
  ProcHandle _ -> False
  Lambda{} -> False
  List xs -> all hashable xs
  Vec xs -> all hashable xs
  Dict kvs -> getAll $ Map.foldMapWithKey (\k v -> All (hashable k && hashable v)) kvs

-- | Smart constructor for dicts which checks that all the keys are hashable.
dict :: (Monad m) => Map Value Value -> Lang m Value
dict kvs =
  if all hashable (Map.keys kvs)
  then pure $ Dict kvs
  else throwErrorHere NonHashableKey

{-# COMPLETE Atom, Keyword, String, Number, Boolean, List, Vec, PrimFn, Dict
  , Ref, Handle, ProcHandle, Lambda #-}

type ValueConC t = (HasCallStack, Ann.Annotation t, Copointed t)

pattern Atom :: ValueConC t => Ident -> Annotated t ValueF
pattern Atom i <- (Ann.match -> AtomF i)
    where
    Atom = Ann.annotate . AtomF

pattern Keyword :: ValueConC t => Ident -> Annotated t ValueF
pattern Keyword i <- (Ann.match -> KeywordF i)
    where
    Keyword = Ann.annotate . KeywordF

pattern String :: ValueConC t => Text -> Annotated t ValueF
pattern String i <- (Ann.match -> StringF i)
    where
    String = Ann.annotate . StringF

pattern Number :: ValueConC t => Rational -> Annotated t ValueF
pattern Number i <- (Ann.match -> NumberF i)
    where
    Number = Ann.annotate . NumberF

pattern Boolean :: ValueConC t => Bool -> Annotated t ValueF
pattern Boolean i <- (Ann.match -> BooleanF i)
    where
    Boolean = Ann.annotate . BooleanF

pattern List :: ValueConC t => [Annotated t ValueF] -> Annotated t ValueF
pattern List vs <- (Ann.match -> ListF vs)
    where
    List = Ann.annotate . ListF

pattern Vec :: ValueConC t => Seq (Annotated t ValueF) -> Annotated t ValueF
pattern Vec vs <- (Ann.match -> VecF vs)
    where
    Vec = Ann.annotate . VecF

pattern PrimFn :: ValueConC t => Ident -> Annotated t ValueF
pattern PrimFn i <- (Ann.match -> PrimFnF i)
    where
    PrimFn = Ann.annotate . PrimFnF

pattern Dict :: ValueConC t => Map.Map Value (Annotated t ValueF) -> Annotated t ValueF
pattern Dict vs <- (Ann.match -> DictF vs)
    where
    Dict = Ann.annotate . DictF

pattern Ref :: ValueConC t => Reference -> Annotated t ValueF
pattern Ref i <- (Ann.match -> RefF i)
    where
    Ref = Ann.annotate . RefF

pattern Handle :: ValueConC t => Hdl -> Annotated t ValueF
pattern Handle i <- (Ann.match -> HandleF i)
    where
    Handle = Ann.annotate . HandleF

pattern ProcHandle :: ValueConC t => ProcHdl -> Annotated t ValueF
pattern ProcHandle i <- (Ann.match -> ProcHandleF i)
    where
    ProcHandle = Ann.annotate . ProcHandleF

pattern Lambda :: ValueConC t => [Ident] -> NonEmpty (Annotated t ValueF) -> Env (Annotated t ValueF) -> Annotated t ValueF
pattern Lambda vs exps env <- (Ann.match -> LambdaF vs exps env)
    where
    Lambda vs exps env = Ann.annotate $ LambdaF vs exps env

type UntaggedValue = Annotated Identity ValueF
type Value = Annotated Ann.WithPos ValueF

-- Remove polymorphism
asValue :: Value -> Value
asValue x = x

-- Should just be a prism
isAtom :: Value -> Maybe Ident
isAtom (Atom i) = pure i
isAtom _        = Nothing

instance A.FromJSON Value where
  parseJSON = \case
    A.Number n -> pure $ Number (toRational n)
    A.String s -> pure $ String s
    A.Array ls -> List . toList <$> traverse parseJSON ls
    A.Bool b -> pure $ Boolean b
    A.Null -> pure $ Keyword (Ident "null")
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
    Number n -> A.Number <$> hush (Num.isSci n)
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

pattern Identifier :: Text -> Ident
pattern Identifier t <- Ident t

-- | Convert a text to an identifier.
--
-- Unsafe! Only use this if you know the string at compile-time and know it's a
-- valid identifier. Otherwise, use 'mkIdent'.
unsafeToIdent :: Text -> Ident
unsafeToIdent = Ident

-- | The environment, which keeps all known bindings.
newtype Env s = Env { fromEnv :: Map Ident (Doc.Docd s) }
    deriving (Eq, Ord, Semigroup, Monoid, Show, Read, Generic, Functor, Foldable, Traversable, Serialise)

instance GhcExts.IsList (Env s) where
    type Item (Env s) = (Ident, Maybe Text, s)
    fromList xs = Env . Map.fromList $ [ (i, Doc.Docd d x)| (i, d, x) <- xs ]
    toList e = [ (i, d, x) | (i, Doc.Docd d x) <- GhcExts.toList . fromEnv $ e]

-- | PrimFn mappings. The parameter specifies the monad the primops run in.
newtype PrimFns m = PrimFns { getPrimFns :: Map Ident (Doc.Docd ([Value] -> Lang m Value)) }
  deriving (Semigroup, Monoid)

instance GhcExts.IsList (PrimFns m) where
    type Item (PrimFns m) = (Ident, Maybe Text, [Value] -> Lang m Value)
    fromList xs = PrimFns . Map.fromList $ [ (i, Doc.Docd d x)| (i, d, x) <- xs ]
    toList e = [ (i, d, x) | (i, Doc.Docd d x) <- GhcExts.toList . getPrimFns $ e]

-- | Bindings, either from the env or from the primops.
data Bindings prims = Bindings
    { bindingsEnv        :: Env Value
    , bindingsPrimFns    :: prims
    , bindingsRefs       :: IntMap Value
    , bindingsNextRef    :: Int
    , bindingsHandles    :: IntMap Handle.Handle
    , bindingsNextHandle :: Int
    , bindingsProcHandles :: IntMap ProcessHandle
    , bindingsNextProcHandle :: Int
    } deriving (Functor, Generic)

emptyBindings :: Bindings (PrimFns m)
emptyBindings = Bindings mempty mempty mempty 0 mempty 0 mempty 0

-- | Extract an environment and references from a Radicle value and put
-- them as the current bindings. Primitive functions are not changed.
--
-- Throws 'OtherError' if @value@ cannot be parsed.
--
-- prop> gets bindingsToRadcile >>= setBindings = pure ()
setBindings :: Monad m => Value -> Lang m ()
setBindings value = do
    case bindingsFromRadicle value of
        Left e  -> throwErrorHere $ OtherError $ "Invalid bindings: " <> e
        Right newBindings -> do
            prims <- gets bindingsPrimFns
            handles <- gets bindingsHandles
            nextHandle <- gets bindingsNextHandle
            procHandles <- gets bindingsProcHandles
            nextProcHandle <- gets bindingsNextProcHandle
            put $ newBindings
                { bindingsPrimFns = prims
                , bindingsHandles = handles
                , bindingsNextHandle = nextHandle
                , bindingsProcHandles = procHandles
                , bindingsNextProcHandle = nextProcHandle
                }
  where
    bindingsFromRadicle x = case x of
        Dict d -> do
            env' <- kwLookup "env" d ?? "Expecting 'env' key"
            refs' <- kwLookup "refs" d ?? "Expecting 'refs' key"
            refs <- makeRefs refs'
            env <- envFromRad env'
            pure $ emptyBindings
                { bindingsEnv = env
                , bindingsRefs = refs
                , bindingsNextRef = length refs
                }
        _ -> throwError "Expecting dict"
    makeRefs refs = case refs of
        List ls -> pure (IntMap.fromList $ zip [0..] ls)
        _       -> throwError $ "Expecting dict"
    envFromRad env = case env of
        Dict d -> fmap (Env . Map.fromList)
                $ forM (Map.toList d) $ \(k, v) -> case k of
            Atom i -> (i, ) <$> fromRad v
            k'     -> Left $ "Expecting atom keys. Got: " <> show k'
        _ -> Left "Expecting dict"

-- | Serializes the environment and references into a Radicle value.
--
-- prop> gets bindingsToRadcile >>= setBindings = pure ()
bindingsToRadicle :: Bindings a -> Value
bindingsToRadicle x =
    Dict $ Map.fromList
        [ (Keyword $ Ident "env", env)
        , (Keyword $ Ident "refs", refs)
        ]
  where
    env = Dict . Map.mapKeys Atom . Map.map toRad . fromEnv $ bindingsEnv x
    refs = List $ IntMap.elems (bindingsRefs x)

-- | The environment in which expressions are evaluated.
newtype LangT r m a = LangT
    { fromLangT :: ExceptT (LangError Value) (StateT r m) a }
    deriving (Functor, Applicative, Monad, MonadError (LangError Value), MonadIO, MonadState r)

mapError :: (Functor m) => (LangError Value -> LangError Value) -> LangT r m a -> LangT r m a
mapError f = LangT . withExceptT f . fromLangT

logPos :: (Functor m) => Ann.SrcPos -> LangT r m a -> LangT r m a
logPos loc = mapError (\(LangError stack err) -> LangError (loc:stack) err)

-- | Log the source location associated with a value.
logValPos :: (Functor m) => Value -> LangT r m a -> LangT r m a
logValPos (Ann.Annotated (Ann.WithPos pos _)) = logPos pos

instance MonadTrans (LangT r) where lift = LangT . lift . lift

-- | A monad for language operations specialized to have as state the Bindings
-- with appropriate underlying monad.
type Lang m = LangT (Bindings (PrimFns m)) m

-- | Run a `Lang` computation with the provided bindings. Returns the result as
-- well as the updated bindings.
runLang
    :: Bindings (PrimFns m)
    -> Lang m a
    -> m (Either (LangError Value) a, Bindings (PrimFns m))
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

addBinding :: Ident -> Maybe Text -> Value -> Bindings m -> Bindings m
addBinding i d v b = b
    { bindingsEnv = Env . Map.insert i (Doc.Docd d v) . fromEnv $ bindingsEnv b }

lookupAtomWithDoc :: Monad m => Ident -> Lang m (Doc.Docd Value)
lookupAtomWithDoc i = get >>= \e -> case Map.lookup i . fromEnv $ bindingsEnv e of
    Nothing -> throwErrorHere $ UnknownIdentifier i
    Just x  -> pure x

-- | Lookup an atom in the environment
lookupAtom :: Monad m => Ident -> Lang m Value
lookupAtom = fmap copoint . lookupAtomWithDoc

lookupAtomDoc :: Monad m => Ident -> Lang m (Maybe Text)
lookupAtomDoc = fmap Doc.doc . lookupAtomWithDoc

-- | Lookup a primop.
lookupPrimop :: Monad m => Ident -> Lang m ([Value] -> Lang m Value)
lookupPrimop i = get >>= \e -> case Map.lookup i $ getPrimFns $ bindingsPrimFns e of
    Nothing -> throwErrorHere $ Impossible $ "Unknown primop " <> fromIdent i
    Just v  -> pure (copoint v)

defineAtom :: Monad m => Ident -> Maybe Text -> Value -> Lang m ()
defineAtom i d v = modify $ addBinding i d v

-- * Eval

-- | The buck-passing eval. Uses whatever 'eval' is in scope.
eval :: Monad m => Value -> Lang m Value
eval val = do
    e <- lookupAtom (Ident "eval")
    logValPos e $ do
        st <- gets bindingsToRadicle
        result <- callFn e [val, st]
        case result of
            List [val', newSt] -> do
                setBindings newSt
                pure val'
            _ -> throwErrorHere
               $ OtherError "eval: should return list with value and new env"


-- | The built-in, original, eval.
baseEval :: Monad m => Value -> Lang m Value
baseEval val = logValPos val $ case val of
    Atom i -> lookupAtom i
    List (f:vs) -> f $$ vs
    List xs -> throwErrorHere
        $ WrongNumberOfArgs ("application: " <> show xs)
                            2
                            (length xs)
    Vec xs -> Vec <$> traverse baseEval xs
    Dict mp -> do
        let evalBoth (a,b) = (,) <$> baseEval a <*> baseEval b
        kvs <- traverse evalBoth (Map.toList mp)
        dict $ Map.fromList kvs
    autoquote -> pure autoquote

specialForms :: forall m. (Monad m) => Map Ident ([Value] -> Lang m Value)
specialForms = Map.fromList $ first Ident <$>
  [ ( "fn"
    , \case
          args : b : bs ->
            case args of
              Vec atoms_ -> do
                atoms <- traverse isAtom (toList atoms_) ?? toLangError (SpecialForm "fn" "One of the arguments was not an atom")
                e <- gets bindingsEnv
                pure (Lambda atoms (b :| bs) e)
              _ -> throwErrorHere $ SpecialForm "fn" "First argument must be a vector of argument atoms"
          _ -> throwErrorHere $ SpecialForm "fn" "Need an argument vector and a body"
      )
  , ("quote", \case
          [v] -> pure v
          xs  -> throwErrorHere $ WrongNumberOfArgs "quote" 1 (length xs))
  , ("def", \case
          [Atom name, val] -> def name Nothing val
          [_, _]           -> throwErrorHere $ OtherError "def expects atom for first arg"
          [Atom name, String d, val] -> def name (Just d) val
          xs -> throwErrorHere $ WrongNumberOfArgs "def" 2 (length xs))
    , ( "def-rec"
      , \case
          [Atom name, val] -> defRec name Nothing val
          [_, _]           -> throwErrorHere $ OtherError "def-rec expects atom for first arg"
          [Atom name, String d, val] -> defRec name (Just d) val
          xs               -> throwErrorHere $ WrongNumberOfArgs "def-rec" 2 (length xs)
      )
    , ("do", (lastDef nil <$>) . traverse baseEval)
    , ("catch", \case
          [l, form, handler] -> do
              mlabel <- baseEval l
              handlerclo <- baseEval handler
              case mlabel of
                  -- TODO reify stack
                  Atom label -> baseEval form `catchError` \(LangError _stack e) -> do
                     (thrownLabel, thrownValue) <- errorDataToValue e
                     if thrownLabel == label || label == Ident "any"
                         then handlerclo $$ [thrownValue]
                         else baseEval form
                  _ -> throwErrorHere $ SpecialForm "catch" "first argument must be atom"
          xs -> throwErrorHere $ WrongNumberOfArgs "catch" 3 (length xs))
    , ("if", \case
          [condition, t, f] -> do
            b <- baseEval condition
            -- I hate this as much as everyone that might ever read Haskell, but
            -- in Lisps a lot of things that one might object to are True...
            if b == Boolean False then baseEval f else baseEval t
          xs -> throwErrorHere $ WrongNumberOfArgs "if" 3 (length xs))
    , ( "cond", (cond =<<) . evenArgs "cond" )
    , ( "match", match )
  ]
  where
    cond = \case
      [] -> pure nil
      (c,e):ps -> do
        b <- baseEval c
        if b /= Boolean False
          then baseEval e
          else cond ps

    match = \case
      v : cases -> do
        cs <- evenArgs "match" cases
        v' <- baseEval v
        goMatches v' cs
      _ -> throwErrorHere $ PatternMatchError NoValue

    goMatches _ [] = throwErrorHere (PatternMatchError NoMatch)
    goMatches v ((m, body):cases) = do
      patFn <- baseEval m
      matchPat <- lookupAtom (Ident "match-pat")
      res <- callFn matchPat [patFn, v]
      let res_ = fromRad res
      case res_ of
        Right (Just (Dict binds)) -> do
          b <- bindsToEnv m binds
          addBinds b *> baseEval body
        Right Nothing -> goMatches v cases
        _ -> throwErrorHere $ PatternMatchError (BadBindings m)

    bindsToEnv pat m = do
        is <- traverse isBind (Map.toList m)
        pure $ Env (Map.fromList is)
      where
        isBind (Atom x, v) = pure (x, Doc.Docd Nothing v)
        isBind _ = throwErrorHere $ PatternMatchError (BadBindings pat)

    addBinds e = modify (\s -> s { bindingsEnv = e <> bindingsEnv s })

    def name doc_ val = do
      val' <- baseEval val
      defineAtom name doc_ val'
      pure nil

    defRec name doc_ val = do
      val' <- baseEval val
      case val' of
        Lambda is b e -> do
          let v = Lambda is b (Env . Map.insert name (Doc.Docd doc_ v) . fromEnv $ e)
          defineAtom name doc_ v
          pure nil
        _ -> throwErrorHere $ OtherError "def-rec can only be used to define functions"

-- * From/ToRadicle

type CPA t = (Ann.Annotation t, Copointed t)

class FromRad t a where
  fromRad :: Annotated t ValueF -> Either Text a
  default fromRad :: (CPA t, HasEot a, FromRadG t (Eot a)) => Annotated t ValueF -> Either Text a
  fromRad = fromRadG

instance CPA t => FromRad t () where
    fromRad (Vec Seq.Empty) = pure ()
    fromRad _               = Left "Expecting an empty vector"
instance (CPA t, FromRad t a, FromRad t b) => FromRad t (a,b) where
    fromRad (Vec (x :<| y :<| Seq.Empty)) = (,) <$> fromRad x <*> fromRad y
    fromRad _ = Left "Expecting a vector of length 2"
instance (CPA t, FromRad t a) => FromRad t (Maybe a) where
    fromRad (Vec (Keyword (Ident "just") :<| x :<| Empty)) = Just <$> fromRad x
    fromRad (Keyword (Ident "nothing")) = pure Nothing
    fromRad _ = Left "Expecting `:nothing` or `[:just _]`"
instance FromRad t (Annotated t ValueF) where
  fromRad = pure
instance CPA t => FromRad t Rational where
  fromRad (Number n) = pure n
  fromRad _          = Left "Not a number"
instance CPA t => FromRad t Scientific where
  fromRad = fromRad >=> Num.isSci
instance CPA t => FromRad t Int where
    fromRad = fromRad >=> Num.isInt
instance CPA t => FromRad t Integer where
    fromRad = fromRad >=> Num.isInteger
instance CPA t => FromRad t Text where
    fromRad x = case x of
        String n -> pure n
        _        -> Left "Expecting string"
instance {-# OVERLAPPING #-} CPA t => FromRad t [Char] where
    fromRad x = case x of
        String n -> pure $ toS n
        _        -> Left "Expecting string"
instance CPA t => FromRad t ExitCode where
    fromRad x = case x of
        Keyword (Ident "ok") -> pure $ ExitSuccess
        Vec (Keyword (Ident "error") Seq.:<| errValue Seq.:<| Seq.Empty) -> ExitFailure <$> fromRad errValue
        _ -> Left "Expecting either :ok or [:error errValue]"
instance (CPA t, FromRad t a) => FromRad t [a] where
    fromRad x = case x of
        List xs -> traverse fromRad xs
        Vec  xs -> traverse fromRad (toList xs)
        _       -> Left "Expecting list"
instance CPA t => FromRad t (Doc.Docd (Annotated t ValueF)) where
    fromRad (Dict d) = do
      val <- kwLookup "val" d ?? "Expecting `:val` key"
      doc <- traverse fromRad (kwLookup "doc" d)
      pure (Doc.Docd doc val)
    fromRad _ = Left "Expecting a dict."
instance CPA t => FromRad t CmdSpec where
    fromRad x = case x of
        Vec (Keyword (Ident "shell") Seq.:<| arg Seq.:<| Seq.Empty) ->
            ShellCommand <$> fromRad arg
        Vec (Keyword (Ident "raw") Seq.:<| comm Seq.:<| args Seq.:<| Seq.Empty) ->
            RawCommand <$> fromRad comm <*> fromRad args
        Vec (Keyword (Ident s) Seq.:<| _) ->
            throwError $ "Expecting either :raw or :shell, got: " <> s
        _ ->
            throwError "Expecting vector"
instance CPA t => FromRad t StdStream where
    fromRad x = case x of
        Keyword (Ident "inherit") -> pure Inherit
        Keyword (Ident "create-pipe") -> pure CreatePipe
        Keyword (Ident "no-stream") -> pure NoStream
        _ -> throwError $ "Expecting :inherit, :create-pipe, or :no-stream"
instance CPA t => FromRad t CreateProcess where
    fromRad x = case x of
        Dict d -> do
            cmdspec' <- fromRad =<< (kwLookup "cmdspec" d ?? "Expecting 'cmdspec' key")
            stdin' <- fromRad =<< (kwLookup "stdin" d ?? "Expecting 'stdin' key")
            stdout' <- fromRad =<< (kwLookup "stdout" d ?? "Expecting 'stdout' key")
            stderr' <- fromRad =<< (kwLookup "stderr" d ?? "Expecting 'stderr' key")
            pure CreateProcess
                { cmdspec = cmdspec'
                , cwd = Nothing
                , env = Nothing
                , std_in = stdin'
                , std_out = stdout'
                , std_err = stderr'
                , close_fds = False
                , create_group = False
                , delegate_ctlc = False
                , detach_console = False
                , create_new_console = False
                , new_session = False
                , child_group = Nothing
                , child_user = Nothing
                , use_process_jobs = False
                }
        _ -> throwError "Expecting dictionary"


class ToRad t a where
  toRad :: a -> Annotated t ValueF
  default toRad :: (HasEot a, ToRadG t (Eot a)) => a -> Annotated t ValueF
  toRad =
    toRadG

instance CPA t => ToRad t () where
    toRad _ = Vec Empty
instance CPA t => ToRad t Bool where
    toRad = Boolean
instance (CPA t, ToRad t a, ToRad t b) => ToRad t (a,b) where
    toRad (a,b) = Vec $ toRad a :<| toRad b :<| Empty
instance (CPA t, ToRad t a, ToRad t b, ToRad t c) => ToRad t (a,b,c) where
    toRad (a,b,c) = Vec $ toRad a :<| toRad b :<| toRad c :<| Empty
instance (CPA t, ToRad t a, ToRad t b, ToRad t c, ToRad t d) => ToRad t (a,b,c,d) where
    toRad (a,b,c,d) = Vec $ toRad a :<| toRad b :<| toRad c :<| toRad d :<| Empty
instance (CPA t, ToRad t a) => ToRad t (Maybe a) where
    toRad Nothing  = Keyword (Ident "nothing")
    toRad (Just x) = Vec $ Keyword (Ident "just") :<| toRad x :<| Empty
instance CPA t => ToRad t Int where
    toRad = Number . fromIntegral
instance CPA t => ToRad t Integer where
    toRad = Number . fromIntegral
instance CPA t => ToRad t Scientific where
    toRad = Number . toRational
instance CPA t => ToRad t Text where
    toRad = String
instance {-# OVERLAPPING #-} CPA t => ToRad t [Char] where
    toRad = String . toS
instance CPA t => ToRad t ExitCode where
    toRad x = case x of
        ExitSuccess -> Keyword (Ident "ok")
        ExitFailure c -> Vec $ Seq.fromList [Keyword (Ident "error"), toRad c]
instance ToRad t (Ann.Annotated t ValueF) where
    toRad = identity
instance (CPA t, ToRad t a) => ToRad t [a] where
    toRad xs = Vec . Seq.fromList $ toRad <$> xs
instance (CPA t, ToRad t a) => ToRad t (Map.Map Text a) where
    toRad xs = Dict $ Map.mapKeys String $ toRad <$> xs
instance CPA t => ToRad t (Doc.Docd (Annotated t ValueF)) where
    toRad (Doc.Docd d_ v) = Dict $ Map.fromList $ ( Keyword (Ident "val"), v) : case d_ of
      Just d  -> [ (Keyword (Ident "doc"), toRad d) ]
      Nothing -> []
instance (CPA t) => ToRad t StdStream where
    toRad x = case x of
        Inherit    -> Keyword $ Ident "inherit"
        CreatePipe -> Keyword $ Ident "create-pipe"
        NoStream   -> Keyword $ Ident "no-stream"
        _          -> panic "Cannot convert handle"
instance (CPA t) => ToRad t CmdSpec where
    toRad x = case x of
        ShellCommand comm ->
            let c = toRad comm
            in Vec $ Seq.fromList [Keyword (Ident "shell"), c]
        RawCommand f args ->
            let f' = toRad f
                args' = toRad args
            in Vec $ Seq.fromList [Keyword (Ident "raw"), f', args']

-- * Helpers

-- Loc is the source location of the application.
callFn :: Monad m => Value -> [Value] -> Lang m Value
callFn f vs = case f of
  Lambda bnds body closure ->
      if length bnds /= length vs
          then throwErrorHere $ WrongNumberOfArgs "lambda" (length bnds)
                                                           (length vs)
          else do
              let mappings = GhcExts.fromList (Doc.noDocs $ zip bnds vs)
                  modEnv = mappings <> closure
              NonEmpty.last <$> withEnv (const modEnv)
                                        (traverse baseEval body)
  PrimFn i -> do
    fn <- lookupPrimop i
    fn vs
  _ -> throwErrorHere $ NonFunctionCalled f

-- | Infix evaluation of application (of functions or special forms)
infixr 1 $$
($$) :: Monad m => Value -> [Value] -> Lang m Value
f $$ vs = case f of
    Atom i ->
      case Map.lookup i specialForms of
        Just form -> form vs
        Nothing   -> fnApp
    _ -> fnApp
  where
    fnApp = do
      f' <- baseEval f
      vs' <- traverse baseEval vs
      callFn f' vs'

nil :: Value
nil = List []

quote :: Value -> Value
quote v = List [Atom (Ident "quote"), v]


list :: [Value] -> Value
list vs = List (Atom (Ident "list") : vs)

kwLookup :: Text -> Map Value (Annotated t ValueF) -> Maybe (Annotated t ValueF)
kwLookup key = Map.lookup (Keyword $ Ident key)

(??) :: MonadError e m => Maybe a -> e -> m a
a ?? n = n `note` a

hoistEither :: MonadError e m => Either e a -> m a
hoistEither = hoistEitherWith identity

hoistEitherWith :: MonadError e' m => (e -> e') -> Either e a -> m a
hoistEitherWith f (Left e)  = throwError (f e)
hoistEitherWith _ (Right x) = pure x

-- | Some forms/functions expect an even number or arguments.
evenArgs :: MonadError (LangError Value) m => Text -> [b] -> m [(b, b)]
evenArgs name = \case
    [] -> pure []
    [_] -> throwErrorHere . OtherError $ name <> ": expects an even number of arguments"
    x:y:xs -> do
        ps <- evenArgs name xs
        pure ((x,y):ps)

-- * Generic encoding/decoding of Radicle values.

toRadG :: forall a t. (HasEot a, ToRadG t (Eot a)) => a -> Annotated t ValueF
toRadG x = toRadConss (constructors (datatype (Proxy :: Proxy a))) (toEot x)

class ToRadG t a where
  toRadConss :: [Constructor] -> a -> Annotated t ValueF

instance (CPA t, ToRadFields t a, ToRadG t b) => ToRadG t (Either a b) where
  toRadConss (Constructor name fieldMeta : _) (Left fields) =
    case fieldMeta of
      Selectors names ->
        radCons (toS name) . pure . Dict . Map.fromList $
          zip (Keyword . Ident . toS <$> names) (toRadFields fields)
      NoSelectors _ -> radCons (toS name) (toRadFields fields)
      NoFields -> radCons (toS name) []
  toRadConss (_ : r) (Right next) = toRadConss r next
  toRadConss [] _ = panic "impossible"

radCons :: CPA t => Prelude.String -> [Annotated t ValueF] -> Annotated t ValueF
radCons name args = case args of
    [] -> consKw
    _  -> Vec ( consKw :<| Seq.fromList args )
  where
    consKw = Keyword . Ident . Identifier.kebabCons $ name

instance ToRadG t Void where
  toRadConss _ = absurd

class ToRadFields t a where
  toRadFields :: a -> [Annotated t ValueF]

instance (ToRad t a, ToRadFields t as) => ToRadFields t (a, as) where
  toRadFields (x, xs) = toRad x : toRadFields xs

instance ToRadFields t () where
  toRadFields () = []

fromRadG :: forall a t. (CPA t, HasEot a, FromRadG t (Eot a)) => Annotated t ValueF -> Either Text a
fromRadG v = do
  (name, args) <- isRadCons v ?? gDecodeErr "expecting constructor"
  fromEot <$> fromRadConss (constructors (datatype (Proxy :: Proxy a))) name args

class FromRadG t a where
  fromRadConss :: [Constructor] -> Text -> [Annotated t ValueF] -> Either Text a

isRadCons :: CPA t => Annotated t ValueF -> Maybe (Text, [Annotated t ValueF])
isRadCons (Keyword (Ident name))                = pure (name, [])
isRadCons (Vec (Keyword (Ident name) :<| args)) = pure (name, toList args)
isRadCons _                                     = Nothing

gDecodeErr :: Text -> Text
gDecodeErr e = "Couldn't generically decode radicle value: " <> e

instance (FromRadFields t a, FromRadG t b) => FromRadG t (Either a b) where
  fromRadConss (Constructor name fieldMeta : r) name' args = do
    if Identifier.kebabCons (toS name) /= name'
      then Right <$> fromRadConss r name' args
      else Left <$> fromRadFields fieldMeta args
  fromRadConss [] _ _ = panic "impossible"

instance FromRadG t Void where
  fromRadConss _ name _ = Left (gDecodeErr "unknown constructor '" <> name <> "'")

class FromRadFields t a where
  fromRadFields :: Fields -> [Annotated t ValueF] -> Either Text a

instance (CPA t, FromRad t a, FromRadFields t as) => FromRadFields t (a, as) where
  fromRadFields fields args = case fields of
    NoSelectors _ -> case args of
      v:vs -> do
        x <- fromRad v
        xs <- fromRadFields fields vs
        pure (x, xs)
      _ -> panic "impossible"
    Selectors (n:names) -> case args of
      [Dict d] -> do
        xv <- kwLookup (toS n) d ?? gDecodeErr ("missing field '" <> toS n <> "'")
        x <- fromRad xv
        xs <- fromRadFields (Selectors names) args
        pure (x, xs)
      _ -> Left . gDecodeErr $ "expecting a dict"
    _ -> panic "impossible"

instance FromRadFields t () where
  fromRadFields _ _ = pure ()
