{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- | The core radicle datatypes and functionality.
module Radicle.Internal.Core where

import qualified Prelude
import           Protolude hiding (Constructor, Handle, State, TypeError, (<>))

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
                 (ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.State hiding (State)
import           Data.Copointed (Copointed(..))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Data.Semigroup ((<>))
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Generics.Eot
import qualified GHC.Exts as GhcExts
import qualified GHC.IO.Handle as Handle
import           System.Process
import qualified Text.Megaparsec.Error as Par

import           Radicle.Internal.Annotation (Annotated)
import qualified Radicle.Internal.Annotation as Ann
import qualified Radicle.Internal.Doc as Doc
import           Radicle.Internal.Identifier (Ident(..))
import qualified Radicle.Internal.Identifier as Identifier
import qualified Radicle.Internal.Number as Num
import           Radicle.Internal.Orphans ()
import           Radicle.Internal.Type (Type(..))
import qualified Radicle.Internal.Type as Type


-- * Value

data LangError r = LangError [Ann.SrcPos] (LangErrorData r)
    deriving (Eq, Show, Generic, Functor)

data PatternMatchError
  = NoMatch
  | NoValue
  | BadBindings Value
  deriving (Eq, Show, Read, Generic)

instance Serialise PatternMatchError

data ModuleError
  = MissingDeclaration
  | InvalidDeclaration Text Value
  | UndefinedExports Ident [Ident]
  deriving (Eq, Show, Read, Generic)

instance Serialise ModuleError

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
    | NonHashableKey Value
    | ModuleError ModuleError
    | OtherError Text
    | ParseError (Par.ParseErrorBundle Text Void)
    -- | Raised if @(throw ident value)@ is evaluated. Arguments are
    -- provided by the call to @throw@.
    | ThrownError Ident r
    | SendError Text
    | PatternMatchError PatternMatchError
    | DaemonError Text
    -- | Raised if the effectful @exit!@ primitive is evaluated.
    | Exit Int
    deriving (Eq, Show, Generic, Functor)

throwErrorHere :: (MonadError (LangError Value) m, HasCallStack) => LangErrorData Value -> m a
throwErrorHere = withFrozenCallStack (throwError . LangError [Ann.thisPos])

toLangError :: (HasCallStack) => LangErrorData Value -> LangError Value
toLangError = LangError [Ann.thisPos]

-- | Remove callstack information.
noStack :: Either (LangError Value) a -> Either (LangErrorData Value) a
noStack (Left (LangError _ err)) = Left err
noStack (Right v)                = Right v

typeToValue :: Type.Type -> Value
typeToValue = Keyword . Ident . Identifier.kebabCons . Type.typeString

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
    NonHashableKey k -> makeVal ("non-hashable-key", [("key", k)])
    ModuleError me -> case me of
      MissingDeclaration -> makeVal ( "missing-module-declaration", [])
      InvalidDeclaration t v -> makeVal ( "invalid-module-declaration", [("info", String t), ("declaration", v)])
      UndefinedExports n is -> makeVal ( "undefined-module-exports"
                                       , [ ("undefined-exports", Vec (Seq.fromList (Atom <$> is)))
                                         , ("module", Atom n) ]
                                       )
    OtherError i -> makeVal
        ( "other-error"
        , [("info", String i)]
        )
    DaemonError i -> makeVal ( "daemon-error", [("info", String i)])
    ParseError _ -> makeVal ("parse-error", [])
    ThrownError label val -> pure (label, val)
    PatternMatchError pe -> case pe of
      NoValue       -> makeVal ( "no-value-to-match", [])
      NoMatch       -> makeVal ( "non-exhaustive-pattern-matches", [])
      BadBindings p -> makeVal ( "bad-pattern", [("bad-pattern", p)])
    SendError se -> makeVal ( "send-error", [("info", String se)] )
    Exit code -> makeVal ("exit", [("code", toRad code)])
  where
    makeA = Atom
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
data Hdl
    = Hdl Int
    | StdIn
    | StdOut
    | StdErr
    deriving (Show, Read, Ord, Eq, Generic)

instance Serialise Hdl

-- | Create a new handle.
newHandle :: Monad m => Handle.Handle -> Lang m Value
newHandle h = do
    b <- get
    let ix = bindingsNextHandle b
    put $ b { bindingsNextHandle = succ ix
            , bindingsHandles = IntMap.insert ix h $ bindingsHandles b
            }
    pure $ Handle $ Hdl ix

-- | Lookup a handle in the bindings, failing if it does not exist.
lookupHandle :: Monad m => Hdl -> Lang m Handle.Handle
lookupHandle (Hdl h) = do
    hdls <- gets bindingsHandles
    case IntMap.lookup h hdls of
        Nothing -> throwErrorHere $ Impossible "no such handle"
        Just v  -> pure v
lookupHandle StdIn = pure stdin
lookupHandle StdOut = pure stdout
lookupHandle StdErr = pure stderr

-- | As with References and Hdls, we keep a counter for each handle, and in the
-- environment map those to actual handles.
newtype ProcHdl = ProcHdl { getProcHandle :: Int }
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

-- | Lookup a process handle in the bindings, failing if it does not exist.
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
    | LambdaF LambdaArgs (NonEmpty r) (Env r) Ident
    -- | Like 'LambdaF' but indicates a function that can call itself
    -- recursively.
    --
    -- The first argument is the name for recursive calls to the
    -- function in the body.
    | LambdaRecF Ident LambdaArgs (NonEmpty r) (Env r) Ident
    | MacroF r
    | VEnvF (Env r)
    | VStateF State
    deriving (Eq, Ord, Read, Show, Generic, Functor)

instance Serialise r => Serialise (ValueF r)

data LambdaArgs =
      PosArgs [Ident]
    | VarArgs Ident
    deriving (Eq, Ord, Read, Show, Generic)

instance Serialise LambdaArgs

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
  LambdaRec{} -> TFunction
  Macro{} -> TMacro
  VEnv _ -> TEnv
  VState _ -> TState

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
  LambdaRec{} -> False
  Macro{} -> False
  VEnv _ -> False
  VState _ -> False
  List xs -> all hashable xs
  Vec xs -> all hashable xs
  Dict kvs -> getAll $ Map.foldMapWithKey (\k v -> All (hashable k && hashable v)) kvs

-- | Smart constructor for dicts which checks that all the keys are hashable.
dict :: (Monad m) => Map Value Value -> Lang m Value
dict kvs = case filter (not . hashable) (Map.keys kvs) of
  []    -> pure $ Dict kvs
  k : _ -> throwErrorHere $ NonHashableKey k

{-# COMPLETE Atom, Keyword, String, Number, Boolean, List, Vec, PrimFn, Dict, Macro
  , Ref, Handle, ProcHandle, Lambda, LambdaRec, VEnv, VState #-}

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

pattern Macro :: ValueConC t => Annotated t ValueF -> Annotated t ValueF
pattern Macro f <- (Ann.match -> MacroF f)
    where
    Macro f = Ann.annotate $ MacroF f

pattern Lambda :: ValueConC t => LambdaArgs -> NonEmpty (Annotated t ValueF) -> Env (Annotated t ValueF) -> Ident -> Annotated t ValueF
pattern Lambda vs exps env ns <- (Ann.match -> LambdaF vs exps env ns)
    where
    Lambda vs exps env ns = Ann.annotate $ LambdaF vs exps env ns

pattern LambdaRec :: ValueConC t => Ident -> LambdaArgs -> NonEmpty (Annotated t ValueF) -> Env (Annotated t ValueF) -> Ident -> Annotated t ValueF
pattern LambdaRec self vs exps env ns <- (Ann.match -> LambdaRecF self vs exps env ns)
    where
    LambdaRec self vs exps env ns = Ann.annotate $ LambdaRecF self vs exps env ns

pattern VEnv :: ValueConC t => Env (Annotated t ValueF) -> Annotated t ValueF
pattern VEnv e <- (Ann.match -> VEnvF e)
  where
    VEnv e = Ann.annotate $ VEnvF e

pattern VState :: ValueConC t => State -> Annotated t ValueF
pattern VState s <- (Ann.match -> VStateF s)
  where
    VState s = Ann.annotate $ VStateF s

type UntaggedValue = Annotated Identity ValueF
type Value = Annotated Ann.WithPos ValueF

-- Remove polymorphism
asValue :: Value -> Value
asValue x = x

-- Should just be a prism
isAtom :: Value -> Maybe Ident
isAtom (Atom i) = pure i
isAtom _        = Nothing

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

data State = State
  { stateEnv  :: Env Value
  , stateRefs :: IntMap Value
  } deriving (Eq, Ord, Read, Show, Generic)

instance Serialise State

data NamespaceBinding
  = Here (Doc.Docd Value)
  | There Ident Ident

type Namespace = Map Ident NamespaceBinding
type Namespaces = Map Ident (Doc.Docd Namespace)

-- | Bindings, either from the env or from the primops.
data Bindings prims = Bindings
    { bindingsEnv            :: Env Value
    , bindingsNamespaces     :: Namespaces
    , bindingsCurrentNamespace :: Ident
    , bindingsPrimFns        :: prims
    , bindingsRefs           :: IntMap Value
    , bindingsNextRef        :: Int
    , bindingsHandles        :: IntMap Handle.Handle
    , bindingsNextHandle     :: Int
    , bindingsProcHandles    :: IntMap ProcessHandle
    , bindingsNextProcHandle :: Int
    } deriving (Functor, Generic)

emptyBindings :: Env Value -> Bindings (PrimFns m)
emptyBindings e = Bindings e mempty (Ident "toplevel") mempty mempty 0 mempty 0 mempty 0

-- | Extract an environment and references from a Radicle value and put
-- them as the current bindings. Primitive functions are not changed.
--
-- Throws 'OtherError' if @value@ cannot be parsed.
--
-- Satisfies the expected properties with `bindingsToRadicle`:
--    * `gets bindingsToRadicle >>= setBindings == pure ()`
--    * `setBindings x >> gets bindingsToRadicle == setBindings x >> pure x`
setBindings :: Monad m => Value -> Lang m ()
setBindings value = do
    case bindingsFromRadicle value of
        Left e  -> throwErrorHere $ OtherError $ "Invalid bindings: " <> e
        Right newBindings -> do
            bnds <- get
            put $ newBindings
                { bindingsPrimFns = bindingsPrimFns bnds
                , bindingsHandles = bindingsHandles bnds
                , bindingsNextHandle = bindingsNextHandle bnds
                , bindingsProcHandles = bindingsProcHandles bnds
                , bindingsNextProcHandle = bindingsNextProcHandle bnds
                }

bindingsFromRadicle :: Value -> Either Text (Bindings (PrimFns m))
bindingsFromRadicle x = case x of
    VState s -> pure $ (emptyBindings (stateEnv s))
                { bindingsRefs = stateRefs s
                , bindingsNextRef = length (stateRefs s)
                }
    _ -> throwError "Expecting state"

-- | Serializes the environment and references into a Radicle value.
--
-- Satisfies the expected properties with `setBindings`:
--    * `gets bindingsToRadicle >>= setBindings == pure ()`
--    * `setBindings x >> gets bindingsToRadicle == setBindings x >> pure x`
bindingsToRadicle :: Bindings a -> Value
bindingsToRadicle x = VState State{ stateEnv = bindingsEnv x, stateRefs = bindingsRefs x }

-- | The environment in which expressions are evaluated.
newtype LangT r m a = LangT
    { fromLangT :: ExceptT (LangError Value) (StateT r m) a }
    deriving (Functor, Applicative, Monad, MonadError (LangError Value)
             , MonadIO, MonadState r)

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
withEnv :: Monad m => (Ident -> Ident) -> (Env Value -> Env Value) -> Lang m a -> Lang m a
withEnv modifyNs modifier action = do
    oldEnv <- gets bindingsEnv
    oldCns <- gets bindingsCurrentNamespace
    modify $ \s -> s { bindingsEnv = modifier oldEnv
                     , bindingsCurrentNamespace = modifyNs oldCns }
    res <- action
    modify $ \s -> s { bindingsEnv = oldEnv
                     , bindingsCurrentNamespace = oldCns }
    pure res

-- * Functions

callExample :: Text -> Value -> Maybe Text
callExample name value = case value of
    Lambda args _ _ _      -> Just $ lambdaDoc args
    --LambdaRec _ args _ _ _ -> Just $ lambdaDoc args
    _                      -> Nothing
  where
    lambdaDoc args = "(" <> T.intercalate " " (name : lambdaArgsDoc args) <> ")"
    lambdaArgsDoc args =
      case args of
        PosArgs argNames ->
          map fromIdent argNames
        VarArgs _ ->
          pure "arg1 ..."

-- TODO(james): improve error messages.
lookupAtomWithDoc :: forall m. Monad m => Ident -> Lang m (Doc.Docd Value)
lookupAtomWithDoc i@(Ident name) =
  get >>= \e -> case Map.lookup i . fromEnv $ bindingsEnv e of
    Just x -> docd x
    Nothing -> lookupInNamespace (bindingsNamespaces e) (bindingsCurrentNamespace e) i
  where
    doc d_ v t = Doc.Docd (Just (t <> maybe "" ("\n\n" <>) d_)) v
    docd x@(Doc.Docd d_ v) = pure (maybe x (doc d_ v) $ callExample name v)

    lookupInNamespace :: Namespaces -> Ident -> Ident -> Lang m (Doc.Docd Value)
    lookupInNamespace nss nsk j = case Map.lookup nsk nss of
      Just (Doc.Docd _ ns) -> case Map.lookup j ns of
        Just (Here x) -> docd x
        Just (There a b) -> lookupInNamespace nss a b
        Nothing -> throwErrorHere $ OtherError $ "symbol " <> show j <> " doesn't exist in namespace " <> show nsk
      Nothing -> throwErrorHere $ OtherError $ "namespace doesn't exist: " <> show nsk

-- | Lookup an atom in the environment
lookupAtom :: Monad m => Ident -> Lang m Value
lookupAtom = fmap copoint . lookupAtomWithDoc

lookupAtomDoc :: Monad m => Ident -> Lang m (Maybe Text)
lookupAtomDoc = fmap Doc.doc . lookupAtomWithDoc

missingDocMsg :: Ident -> Text
missingDocMsg i = "No documentation found for " <> fromIdent i <> "."

-- | Lookup a primop.
lookupPrimop :: Monad m => Ident -> Lang m ([Value] -> Lang m Value)
lookupPrimop i = get >>= \e -> case Map.lookup i $ getPrimFns $ bindingsPrimFns e of
    Nothing -> throwErrorHere $ Impossible $ "Unknown primop " <> fromIdent i
    Just v  -> pure (copoint v)

modifyCurrentNamespace :: Monad m => (Namespace -> Namespace) -> Lang m ()
modifyCurrentNamespace f = do
    b@Bindings{ bindingsCurrentNamespace = cns
              , bindingsNamespaces = nss } <- get
    nss' <- Map.alterF f' cns nss
    put $ b { bindingsNamespaces = nss' }
  where
    f' Nothing = throwErrorHere $ OtherError "namespace missing!" -- TODO(james): make better
    f' (Just ns) = pure $ Just (f <$> ns)

defineAtomInNs :: Monad m => Ident -> Maybe Text -> Value -> Lang m ()
defineAtomInNs i d v = modifyCurrentNamespace (Map.insert i (Here (Doc.Docd d v)))

defineAtom :: Monad m => Ident -> Maybe Text -> Value -> Lang m ()
defineAtom i d v = modify addBinding
  where
    addBinding b = b
      { bindingsEnv = Env . Map.insert i (Doc.Docd d v) . fromEnv $ bindingsEnv b }

-- * From/ ToRadicle

type CPA t = (Ann.Annotation t, Copointed t)

class FromRad t a where
  fromRad :: (CPA t) => Annotated t ValueF -> Either Text a
  default fromRad :: (CPA t, HasEot a, FromRadG t (Eot a)) => Annotated t ValueF -> Either Text a
  fromRad = fromRadG

instance FromRad t () where
    fromRad (Vec Seq.Empty) = pure ()
    fromRad _               = Left "Expecting an empty vector"
instance (FromRad t a, FromRad t b) => FromRad t (a,b) where
    fromRad (Vec (x :<| y :<| Seq.Empty)) = (,) <$> fromRad x <*> fromRad y
    fromRad _ = Left "Expecting a vector of length 2"
instance (FromRad t a) => FromRad t (Maybe a) where
    fromRad (Vec (Keyword (Ident "just") :<| x :<| Empty)) = Just <$> fromRad x
    fromRad (Keyword (Ident "nothing")) = pure Nothing
    fromRad _ = Left "Expecting `:nothing` or `[:just _]`"
instance FromRad t (Annotated t ValueF) where
  fromRad = pure
instance FromRad t Rational where
  fromRad (Number n) = pure n
  fromRad _          = Left "Not a number"
instance FromRad t Scientific where
  fromRad = fromRad >=> Num.isSci
instance FromRad t Int where
    fromRad = fromRad >=> Num.isInt
instance FromRad t Integer where
    fromRad = fromRad >=> Num.isInteger
instance FromRad t Text where
    fromRad x = case x of
        String n -> pure n
        _        -> Left "Expecting string"
instance {-# OVERLAPPING #-} FromRad t [Char] where
    fromRad x = case x of
        String n -> pure $ toS n
        _        -> Left "Expecting string"
instance FromRad t ExitCode where
    fromRad x = case x of
        Keyword (Ident "ok") -> pure $ ExitSuccess
        Vec (Keyword (Ident "error") Seq.:<| errValue Seq.:<| Seq.Empty) -> ExitFailure <$> fromRad errValue
        _ -> Left "Expecting either :ok or [:error errValue]"
instance (FromRad t a) => FromRad t [a] where
    fromRad x = case x of
        List xs -> traverse fromRad xs
        Vec  xs -> traverse fromRad (toList xs)
        _       -> Left "Expecting list or vector"
instance FromRad t (Doc.Docd (Annotated t ValueF)) where
    fromRad (Dict d) = do
      val <- kwLookup "val" d ?? "Expecting `:val` key"
      doc <- traverse fromRad (kwLookup "doc" d)
      pure (Doc.Docd doc val)
    fromRad _ = Left "Expecting a dict."
instance FromRad t CmdSpec where
    fromRad x = case x of
        Vec (Keyword (Ident "shell") Seq.:<| arg Seq.:<| Seq.Empty) ->
            ShellCommand <$> fromRad arg
        Vec (Keyword (Ident "raw") Seq.:<| comm Seq.:<| Vec args Seq.:<| Seq.Empty) -> do
            args' <- traverse fromRad $ toList args
            comm' <- fromRad comm
            pure $ RawCommand comm' args'
        Vec (Keyword (Ident s) Seq.:<| _) ->
            throwError $ "Expecting either :raw or :shell, got: " <> s
        _ ->
            throwError "Expecting vector"
instance FromRad t StdStream where
    fromRad x = case x of
        Keyword (Ident "inherit") -> pure Inherit
        Keyword (Ident "create-pipe") -> pure CreatePipe
        Keyword (Ident "no-stream") -> pure NoStream
        _ -> throwError $ "Expecting :inherit, :create-pipe, or :no-stream"
instance FromRad t CreateProcess where
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
  toRad :: CPA t => a -> Annotated t ValueF
  default toRad :: (HasEot a, ToRadG t (Eot a)) => a -> Annotated t ValueF
  toRad = toRadG

instance ToRad t () where
    toRad _ = Vec Empty
instance ToRad t Bool where
    toRad = Boolean
instance (ToRad t a, ToRad t b) => ToRad t (a,b) where
    toRad (a,b) = Vec $ toRad a :<| toRad b :<| Empty
instance (ToRad t a, ToRad t b, ToRad t c) => ToRad t (a,b,c) where
    toRad (a,b,c) = Vec $ toRad a :<| toRad b :<| toRad c :<| Empty
instance (ToRad t a, ToRad t b, ToRad t c, ToRad t d) => ToRad t (a,b,c,d) where
    toRad (a,b,c,d) = Vec $ toRad a :<| toRad b :<| toRad c :<| toRad d :<| Empty
instance (ToRad t a) => ToRad t (Maybe a) where
    toRad Nothing  = Keyword (Ident "nothing")
    toRad (Just x) = Vec $ Keyword (Ident "just") :<| toRad x :<| Empty
instance ToRad t Int where
    toRad = Number . fromIntegral
instance ToRad t Integer where
    toRad = Number . fromIntegral
instance ToRad t Scientific where
    toRad = Number . toRational
instance ToRad t Text where
    toRad = String
instance {-# OVERLAPPING #-} ToRad t [Char] where
    toRad = String . toS
instance ToRad t ExitCode where
    toRad x = case x of
        ExitSuccess -> Keyword (Ident "ok")
        ExitFailure c -> Vec $ Seq.fromList [Keyword (Ident "error"), toRad c]
instance ToRad t (Ann.Annotated t ValueF) where
    toRad = identity
instance (ToRad t a) => ToRad t [a] where
    toRad xs = Vec . Seq.fromList $ toRad <$> xs
instance (ToRad t a) => ToRad t (Map.Map Text a) where
    toRad xs = Dict $ Map.mapKeys String $ toRad <$> xs
instance ToRad t (Doc.Docd (Annotated t ValueF)) where
    toRad (Doc.Docd d_ v) = Dict $ Map.fromList $ ( Keyword (Ident "val"), v) : case d_ of
      Just d  -> [ (Keyword (Ident "doc"), toRad d) ]
      Nothing -> []
instance ToRad t StdStream where
    toRad x = case x of
        Inherit    -> Keyword $ Ident "inherit"
        CreatePipe -> Keyword $ Ident "create-pipe"
        NoStream   -> Keyword $ Ident "no-stream"
        _          -> panic "Cannot convert handle"
instance ToRad t CmdSpec where
    toRad x = case x of
        ShellCommand comm ->
            let c = toRad comm
            in Vec $ Seq.fromList [Keyword (Ident "shell"), c]
        RawCommand f args ->
            let f' = toRad f
                args' = toRad args
            in Vec $ Seq.fromList [Keyword (Ident "raw"), f', args']

-- * Helpers

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
toRadG x = toRadConss (case conss of {[_] -> SingleCons; _ -> NonSingleCons})
                      conss
                      (toEot x)
  where
    conss = constructors (datatype (Proxy :: Proxy a))

data ConstructorMultiplicity = SingleCons | NonSingleCons

class ToRadG t a where
  toRadConss :: ConstructorMultiplicity -> [Constructor] -> a -> Annotated t ValueF

consArgs :: (CPA t , ToRadFields t a) => Fields -> a -> [Annotated t ValueF]
consArgs fieldMeta fields =
  case fieldMeta of
    Selectors names ->
      [Dict . Map.fromList $ zip (Keyword . Ident . toS <$> names) (toRadFields fields)]
    NoSelectors _ -> toRadFields fields
    NoFields -> []

instance (CPA t, ToRadFields t a, ToRadG t b) => ToRadG t (Either a b) where
  toRadConss SingleCons [Constructor name fieldMeta] (Left fields) = case consArgs fieldMeta fields of
    [a] -> a
    as  -> radCons (toS name) as
  toRadConss NonSingleCons (Constructor name fieldMeta : _) (Left fields) = radCons (toS name) $ consArgs fieldMeta fields
  toRadConss NonSingleCons (_ : r) (Right next) = toRadConss NonSingleCons r next
  toRadConss _ _ _ = panic "impossible"

radCons :: CPA t => Prelude.String -> [Annotated t ValueF] -> Annotated t ValueF
radCons name args = case args of
    [] -> consKw
    _  -> Vec ( consKw :<| Seq.fromList args )
  where
    consKw = Keyword . Ident . Identifier.kebabCons $ name

instance ToRadG t Void where
  toRadConss _ _ = absurd

class ToRadFields t a where
  toRadFields :: CPA t => a -> [Annotated t ValueF]

instance (ToRad t a, ToRadFields t as) => ToRadFields t (a, as) where
  toRadFields (x, xs) = toRad x : toRadFields xs

instance ToRadFields t () where
  toRadFields () = []

fromRadG :: forall a t. (CPA t, HasEot a, FromRadG t (Eot a)) => Annotated t ValueF -> Either Text a
fromRadG v = case isRadCons v of
    Nothing -> case conss of
      [Constructor name _] -> fromEot <$> fromRadConss conss (Identifier.kebabCons (toS name)) [v]
      _ -> Left $ gDecodeErr "expecting constructor: Haskell type is not single-constructor"
    Just (name, args) -> fromEot <$> fromRadConss conss name args
  where
    conss = constructors (datatype (Proxy :: Proxy a))
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
