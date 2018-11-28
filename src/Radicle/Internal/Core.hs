{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The core radicle datatypes and functionality.
module Radicle.Internal.Core where

import           Protolude hiding (Constructor, TypeError, (<>))

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
                 (ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.State
import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as A
import           Data.Copointed (Copointed(..))
import qualified Data.Data as Data
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
import qualified GHC.IO.Handle as Handle
import           System.Process (CmdSpec(..), CreateProcess(..), StdStream(..))
import qualified Text.Megaparsec.Error as Par

import qualified Radicle.Internal.Annotation as Ann
import qualified Radicle.Internal.Doc as Doc
import qualified Radicle.Internal.Identifier as Identifier
import qualified Radicle.Internal.Number as Num
import           Radicle.Internal.Orphans ()


-- * Value

data LangError r = LangError [Ann.SrcPos] (LangErrorData r)
    deriving (Eq, Show, Read, Generic, Functor)

instance Serialise r => Serialise (LangError r)

-- | An error throw during parsing or evaluating expressions in the language.
data LangErrorData r =
      UnknownIdentifier Ident
    | Impossible Text
    | TypeError Text
    -- | Takes the function name, expected number of args, and actual number of
    -- args
    | WrongNumberOfArgs Text Int Int
    | NonHashableKey
    | OtherError Text
    | ParseError (Par.ParseError Char Void)
    | ThrownError Ident r
    | Exit
    deriving (Eq, Show, Read, Generic, Functor)

instance Serialise r => Serialise (LangErrorData r)

throwErrorHere :: (MonadError (LangError (Value b)) m, HasCallStack) => LangErrorData (Value b) -> m a
throwErrorHere = withFrozenCallStack (throwError . LangError [])

toLangError :: LangErrorData (Value vt) -> LangError (Value vt)
toLangError = LangError []

-- | Remove callstack information.
noStack :: Either (LangError (Value vt)) a -> Either (LangErrorData (Value vt)) a
noStack (Left (LangError _ err)) = Left err
noStack (Right v)                = Right v

-- | Convert an error to a radicle value, and the label for it. Used for
-- catching exceptions.
errorDataToValue
    :: Monad m
    => LangErrorData (Value')
    -> Lang m (Ident, Value')
errorDataToValue e = case e of
    UnknownIdentifier i -> makeVal
        ( "unknown-identifier"
        , [("identifier", makeA i)]
        )
    -- "Now more than ever seems it rich to die"
    Impossible _ -> throwErrorHere e
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
    NonHashableKey -> makeVal ( "non-hashable-key", [])
    OtherError i -> makeVal
        ( "other-error"
        , [("info", String i)]
        )
    ParseError _ -> makeVal ("parse-error", [])
    ThrownError label val -> pure (label, val)
    Exit -> makeVal ("exit", [])
  where
    makeA = quote . Atom ()
    makeVal (t,v) = pure (Ident t, Dict () $ Map.mapKeys (Keyword () . Ident) . GhcExts.fromList $ v)

newtype Reference = Reference { getReference :: Int }
    deriving (Show, Read, Ord, Eq, Generic, Serialise)

-- | Create a new ref with the supplied initial value.
newRef :: Monad m => Value' -> Lang m Value'
newRef v = do
    b <- get
    let ix = bindingsNextRef b
    put $ b { bindingsNextRef = succ ix
            , bindingsRefs = IntMap.insert ix v $ bindingsRefs b
            }
    pure . Ref $ Reference ix

-- | Read the value of a reference.
readRef :: Monad m => Reference -> Lang m Value'
readRef (Reference r) = do
    refs <- gets bindingsRefs
    case IntMap.lookup r refs of
        Nothing -> throwErrorHere $ Impossible "undefined reference"
        Just v  -> pure v

data Handle' = Handle'
    { handleCounter :: Int
    , handleHandle  :: Handle.Handle
    } deriving (Eq, Show)

instance Ord Handle' where
    (<=) = (<=) `on` handleCounter

-- | An expression or value in the language.
-- We use a variation of the Trees that Grow approach with the type parameter.
-- Most obviously, Void means the constructor can't be used (and we make the
-- 'Extra' field strict to ensure that). Additionally, we parametrize a single
-- 'Extra' type family by constructor name (which unfortunately can't be the
-- promoted constructor) and keep various "aspects" in the second parameter
-- under the 'Is' type.
data Value x =
    -- | A regular (hyperstatic) variable.
      Atom !(Extra "atom" x) Ident
    -- | Symbolic identifiers that evaluate to themselves.
    | Keyword !(Extra "keyword" x) Ident
    | String !(Extra "string" x) Text
    | Number !(Extra "number" x) Rational
    | Boolean !(Extra "boolean" x) Bool
    | List !(Extra "list" x) [Value x]
    | Vec !(Extra "vec" x) (Seq (Value x))
    | PrimFn !(Extra "primfn" x) Ident
    -- | Map from *pure* Values -- annotations shouldn't change lookup semantics.
    | Dict !(Extra "dict" x) (Map.Map Data (Value x))
    | Ref !(Extra "ref" x) Reference
    -- | A file handle.
    | Handle !(Extra "handle" x) Handle'
    -- | Takes the arguments/parameters, a body, and possibly a closure.
    --
    -- The value of an application of a lambda is always the last value in the
    -- body. The only reason to have multiple values is for effects.
    | Lambda !(Extra "lambda" x) [Ident] (NonEmpty (Value x)) (Env (Value x))
    deriving (Generic)

type AllAre (f :: * -> Constraint) x =
   ( f (Extra "atom" x), f (Extra "keyword" x), f (Extra "string" x)
   , f (Extra "number" x), f (Extra "boolean" x), f (Extra "list" x)
   , f (Extra "vec" x), f (Extra "primfn" x), f (Extra "dict" x)
   , f (Extra "ref" x) , f (Extra "handle" x), f (Extra "lambda" x)
   )
instance AllAre Serialise x => Serialise (Value x)
instance AllAre Eq x => Eq (Value x)
instance AllAre Show x => Show (Value x)
instance AllAre Ord x => Ord (Value x)

-- | Whether a 'Value' is annotated or not.
data IsAnnotated
    = Annotated
    | NotAnnotated

-- | How "data-like" a value is.
data IsData
    = Data        -- ^ Something that is Data can be sensibly serialized, and
                  -- defines our own EDN/JSON format, as well as being allowed
                  -- as dict keys
    | NotData     -- ^ Includes things like lambdas, refs, and handles, which
                  -- should not in general be serialized, and cannot be keys to
                  -- dicts.
    | NotPure     -- ^ Values that shouldn't exist in pure environments - e.g.
                  -- Handle. These are a superset of NotData.

data Is (hashable :: IsData) (annotated :: IsAnnotated)
type Data = Value (Is 'Data 'NotAnnotated)
type Expr = Value (Is 'NotPure 'Annotated)
type Value' = Value (Is 'NotPure 'NotAnnotated)
type PureNotAnnotValue = Value (Is 'NotData 'NotAnnotated)
type AnyValue = forall a b. Value (Is a b)

-- | Extra indicates what the additional field contains, for each type of
-- Value.
-- We use a closed type family so we can have overlaps, but these can be made
-- open with a little more typing if we need the extra modularity.
type family Extra (con :: Symbol) stage where
    Extra "handle" (Is 'NotPure a)      = ()
    Extra "handle" (Is x a)             = Void
    Extra "primfn" (Is 'Data b)         = Void
    Extra "primfn" (Is x b)             = ()
    Extra "lambda" (Is 'Data b)         = Void
    Extra "lambda" (Is x b)             = ()
    Extra "ref"    (Is 'Data b)         = Void
    Extra "ref"    (Is x b)             = Void
    Extra x        (Is b 'Annotated)    = Ann.SrcPos
    Extra x        (Is b 'NotAnnotated) = ()

-- class MkExtra a where
--     type Extra' a
--     mkExtra :: (Extra' a

-- Should just be a prism
isAtom :: Value vt -> Maybe Ident
isAtom (Atom _ i) = pure i
isAtom _          = Nothing

instance A.FromJSON Data where
  parseJSON = \case
    A.Number n -> pure $ Number () (toRational n)
    A.String s -> pure $ String () s
    A.Array ls -> List . toList <$> traverse parseJSON ls
    A.Bool b -> pure $ Boolean () b
    A.Null -> pure $ Keyword () (Ident "null")
    A.Object hm -> do
      let kvs = HashMap.toList hm
      vs <- traverse parseJSON (snd <$> kvs)
      pure . Dict () . Map.fromList $ zip (String . fst <$> kvs) vs

instance A.ToJSON Data where
    toJSON = _

-- | An identifier in the language.
--
-- Not all `Text`s are valid identifiers, so use 'Ident' at your own risk.
-- `mkIdent` is the safe version.
newtype Ident = Ident { fromIdent :: Text }
    deriving (Eq, Show, Read, Ord, Generic, Data.Data, Serialise)

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
newtype PrimFns m
  = PrimFns { getPrimFns :: Map Ident (Doc.Docd ([Value'] -> Lang m Value')) }
  deriving (Semigroup, Monoid)

instance GhcExts.IsList (PrimFns m) where
    type Item (PrimFns m) = (Ident, Maybe Text, [Value'] -> Lang m Value')
    fromList xs = PrimFns . Map.fromList $ [ (i, Doc.Docd d x)| (i, d, x) <- xs ]
    toList e = [ (i, d, x) | (i, Doc.Docd d x) <- GhcExts.toList . getPrimFns $ e]

-- | Bindings, either from the env or from the primops.
data Bindings prims = Bindings
    { bindingsEnv     :: Env (Value (Is 'NotPure 'NotAnnotated))
    , bindingsPrimFns :: prims
    , bindingsRefs    :: IntMap (Value (Is 'NotPure 'NotAnnotated))
    , bindingsNextRef :: Int
    } deriving (Eq, Show, Functor, Generic)

-- | The environment in which expressions are evaluated.
newtype LangT valTyp r m a = LangT
    { fromLangT :: ExceptT (LangError (Value valTyp)) (StateT r m) a }
    deriving (Functor, Applicative, Monad, MonadError (LangError (Value valTyp)), MonadIO, MonadState r)

instance MonadTrans (LangT valTyp r) where lift = LangT . lift . lift

mapError
    :: (Functor m)
    => (LangError (Value vt) -> LangError (Value vt))
    -> LangT vt r m a
    -> LangT vt r m a
mapError f = LangT . withExceptT f . fromLangT


-- | A monad for language operations specialized to have as state the Bindings
-- with appropriate underlying monad.
type Lang m =
    LangT (Is 'NotPure 'NotAnnotated)
          (Bindings (PrimFns m))
          m

-- | Run a `Lang` computation with the provided bindings. Returns the result as
-- well as the updated bindings.
runLang
    :: Bindings (PrimFns m)
    -> Lang m a
    -> m (Either (LangError Value') a, Bindings (PrimFns m))
runLang e l = runStateT (runExceptT $ fromLangT l) e

-- | Like 'local' or 'withState'. Will run an action with a modified environment
-- and then restore the original bindings.
withBindings
    :: Monad m
    => (Bindings (PrimFns m) -> Bindings (PrimFns m))
    -> Lang m a
    -> Lang m a
withBindings modifier action = do
    oldBnds <- get
    modify modifier
    res <- action
    put oldBnds
    pure res

-- | Like 'local' or 'withState'. Will run an action with a modified environment
-- and then restore the original environment. Other bindings (i.e. primops and
-- refs) are not affected.
withEnv :: Monad m => (Env Value' -> Env Value') -> Lang m a -> Lang m a
withEnv modifier action = do
    oldEnv <- gets bindingsEnv
    modify $ \s -> s { bindingsEnv = modifier oldEnv }
    res <- action
    modify $ \s -> s { bindingsEnv = oldEnv }
    pure res

-- * Functions

addBinding :: Ident -> Maybe Text -> Value' -> Bindings m -> Bindings m
addBinding i d v b = b
    { bindingsEnv = Env . Map.insert i (Doc.Docd d v) . fromEnv $ bindingsEnv b }

lookupAtomWithDoc :: Monad m => Ident -> Lang m (Doc.Docd Value')
lookupAtomWithDoc i = get >>= \e -> case Map.lookup i . fromEnv $ bindingsEnv e of
    Nothing -> throwErrorHere $ UnknownIdentifier i
    Just x  -> pure x

-- | Lookup an atom in the environment
lookupAtom :: Monad m => Ident -> Lang m Value'
lookupAtom = fmap copoint . lookupAtomWithDoc

lookupAtomDoc :: Monad m => Ident -> Lang m (Maybe Text)
lookupAtomDoc = fmap Doc.doc . lookupAtomWithDoc

-- | Lookup a primop.
lookupPrimop :: Monad m => Ident -> Lang m ([Value'] -> Lang m Value')
lookupPrimop i = get >>= \e -> case Map.lookup i $ getPrimFns $ bindingsPrimFns e of
    Nothing -> throwErrorHere $ Impossible $ "Unknown primop " <> fromIdent i
    Just v  -> pure (copoint v)

defineAtom :: Monad m => Ident -> Maybe Text -> Value' -> Lang m ()
defineAtom i d v = modify $ addBinding i d v

-- * Eval

-- | The buck-passing eval. Uses whatever 'eval' is in scope.
eval :: Monad m => Value' -> Lang m Value'
eval val = do
    e <- lookupAtom (Ident "eval")
    st <- gets toRad
    logValPos e $ callFn e [val, st] >>= updateEnvAndReturn
  where
    updateEnvAndReturn :: Monad m => Value vt -> Lang vt m (Value vt)
    updateEnvAndReturn v = case v of
        List _ [val', newSt] -> do
            prims <- gets bindingsPrimFns
            newSt' <- either (throwErrorHere . OtherError) pure
                      (fromRad newSt :: Either Text (Bindings ()))
            put $ newSt' { bindingsPrimFns = prims }
            pure val'
        _ -> throwErrorHere $ OtherError "eval: should return list with value and new env"


-- | The built-in, original, eval.
baseEval :: Monad m => Expr -> Lang m Value'
baseEval val = case val of
    Atom _ i -> lookupAtom i
    List _ (f:vs) -> f $$ vs
    List _ xs -> throwErrorHere
        $ WrongNumberOfArgs ("application: " <> show xs)
                            2
                            (length xs)
    Vec a xs -> Vec a <$> traverse baseEval xs
    Dict a mp -> do
        let evalBoth (a,b) = (,) <$> baseEval a <*> baseEval b
        kvs <- traverse evalBoth (Map.toList mp)
        Dict a $ Map.fromList kvs
    autoquote -> pure autoquote

specialForms
    :: forall m. (Monad m)
    => Map Ident ([Value'] -> Lang m Value')
specialForms = Map.fromList $ first Ident <$>
    [ ( "fn"
      , \case
            args : b : bs ->
              case args of
                Vec _ atoms_ -> do
                  atoms <- traverse isAtom (toList atoms_)
                      ?? toLangError (TypeError "fn: expecting a list of symbols")
                  e <- gets bindingsEnv
                  pure (Lambda atoms (b :| bs) e)
                _ -> throwErrorHere $ OtherError
                      "fn: first argument must be a vector of argument symbols,\
                      \ and then at least one form for the body"
            xs -> throwErrorHere $ WrongNumberOfArgs "fn" 2 (length xs) -- TODO: technically "at least 2"
        )
    , ("quote", \case
            [v] -> pure v
            xs  -> throwErrorHere $ WrongNumberOfArgs "quote" 1 (length xs))
    , ("def", \case
            [Atom _ name, val] -> def name Nothing val
            [_, _]             -> throwErrorHere $ OtherError "def expects atom for first arg"
            [Atom _ name, String _ d, val] -> def name (Just d) val
            xs -> throwErrorHere $ WrongNumberOfArgs "def" 2 (length xs))
    , ( "def-rec"
      , \case
          [Atom _ name, val] -> defRec name Nothing val
          [_, _]           -> throwErrorHere $ OtherError "def-rec expects atom for first arg"
          [Atom _ name, String _ d, val] -> defRec name (Just d) val
          xs               -> throwErrorHere $ WrongNumberOfArgs "def-rec" 2 (length xs)
      )
    , ("do", (lastDef nil <$>) . traverse baseEval)
    , ("catch", \case
          [l, form, handler] -> do
              mlabel <- baseEval l
              handlerclo <- baseEval handler
              case mlabel of
                  -- TODO reify stack
                  Atom _ label -> baseEval form `catchError` \(LangError _stack e) -> do
                     (thrownLabel, thrownValue) <- errorDataToValue e
                     if thrownLabel == label || label == Ident "any"
                         then handlerclo $$ [thrownValue]
                         else baseEval form
                  _ -> throwErrorHere $ TypeError "catch: first argument must be atom"
          xs -> throwErrorHere $ WrongNumberOfArgs "catch" 3 (length xs))
    , ("if", \case
          [condition, t, f] -> do
            b <- baseEval condition
            -- I hate this as much as everyone that might ever read Haskell, but
            -- in Lisps a lot of things that one might object to are True...
            if b == Boolean () False then baseEval f else baseEval t
          xs -> throwErrorHere $ WrongNumberOfArgs "if" 3 (length xs))
    , ( "cond", (cond =<<) . evenArgs "cond" )
  ]
  where
    cond = \case
      [] -> pure nil
      (c,e):ps -> do
        b <- baseEval c
        case b of
          Boolean _ False -> cond ps
          _               -> baseEval e

    def name doc_ val = do
      val' <- baseEval val
      defineAtom name doc_ val'
      pure nil

    defRec name doc_ val = do
      val' <- baseEval val
      case val' of
        Lambda _ is b e -> do
          let v = Lambda _ is b (Env . Map.insert name (Doc.Docd doc_ v) . fromEnv $ e)
          defineAtom name doc_ v
          pure nil
        _ -> throwErrorHere $ OtherError "def-rec can only be used to define functions"

-- * From/ToRadicle

class FromRad a where
    fromRad :: Value' -> Either Text a
    default fromRad :: (HasEot a, FromRadG (Eot a)) => Value' -> Either Text a
    fromRad = fromRadG

instance FromRad () where
    fromRad (Vec _ Seq.Empty) = pure ()
    fromRad _                 = Left "Expecting an empty vector"
instance (FromRad a, FromRad b) => FromRad (a,b) where
    fromRad (Vec _ (x :<| y :<| Seq.Empty)) = (,) <$> fromRad x <*> fromRad y
    fromRad _ = Left "Expecting a vector of length 2"
instance (FromRad a) => FromRad (Maybe a) where
    fromRad (Vec _ (Keyword _ (Ident "Just") :<| x :<| Empty)) = Just <$> fromRad x
    fromRad (Keyword _ (Ident "Nothing")) = pure Nothing
    fromRad _ = Left "Expecting :Nothing or [:Just _]"
instance FromRad Value' where
    fromRad = pure
instance FromRad Rational where
    fromRad (Number _ n) = pure n
    fromRad _            = Left "Not a number"
instance FromRad Scientific where
    fromRad = fromRad >=> Num.isSci
instance FromRad Int where
    fromRad = fromRad >=> Num.isInt
instance FromRad Integer where
    fromRad = fromRad >=> Num.isInteger
instance FromRad Text where
    fromRad x = case x of
        String _ n -> pure n
        _          -> Left "Expecting string"
instance {-# OVERLAPPING #-} FromRad [Char] where
    fromRad x = case x of
        String _ n -> pure $ toS n
        _          -> Left "Expecting string"
instance FromRad ExitCode where
    fromRad x = case x of
        Keyword _ (Ident "ok")
            -> pure $ ExitSuccess
        Vec _ (Keyword _ (Ident "error") Seq.:<| errValue Seq.:<| Seq.Empty)
            -> ExitFailure <$> fromRad errValue
        _   -> Left "Expecting either :ok or [:error errValue]"
instance (FromRad a) => FromRad [a] where
    fromRad x = case x of
        List _ xs -> traverse fromRad xs
        Vec  _ xs -> traverse fromRad (toList xs)
        _         -> Left "Expecting list"
instance FromRad (Doc.Docd Value') where
    fromRad (Dict _ d) = do
      val <- kwLookup "val" d ?? "Expecting `:val` key"
      doc <- traverse fromRad (kwLookup "doc" d)
      pure (Doc.Docd doc val)
    fromRad _ = Left "Expecting a dict."
instance FromRad (Env Value') where
    fromRad x = case x of
        Dict _ d -> fmap (Env . Map.fromList)
                $ forM (Map.toList d) $ \(k, v) -> case k of
            Atom _ i -> do
              docd <- fromRad v
              pure (i, docd)
            k'     -> Left $ "Expecting atom keys. Got: " <> show k'
        _ -> Left "Expecting dict"
instance FromRad (Bindings ()) where
    fromRad x = case x of
        Dict _ d -> do
            env' <- kwLookup "env" d ?? "Expecting 'env' key"
            refs' <- kwLookup "refs" d ?? "Expecting 'refs' key"
            refs <- makeRefs refs'
            env <- fromRad env'
            pure $ Bindings env () refs (length refs)
        _ -> throwError "Expecting dict"
      where
        makeRefs refs = case refs of
            List _ ls -> pure (IntMap.fromList $ zip [0..] ls)
            _         -> throwError $ "Expecting dict"
instance FromRad CmdSpec where
    fromRad x = case x of
        Vec _ (Keyword _ (Ident "shell") Seq.:<| arg Seq.:<| Seq.Empty) ->
            ShellCommand <$> fromRad arg
        Vec _ (Keyword _ (Ident "raw") Seq.:<| comm Seq.:<| args Seq.:<| Seq.Empty) ->
            RawCommand <$> fromRad comm <*> fromRad args
        Vec _ (Keyword _ (Ident s) Seq.:<| _) ->
            throwError $ "Expecting either :raw or :shell, got: " <> s
        _ ->
            throwError "Expecting vector"
instance FromRad StdStream where
    fromRad x = case x of
        Keyword _ (Ident "inherit") -> pure Inherit
        Keyword _ (Ident "create-pipe") -> pure CreatePipe
        Keyword _ (Ident "no-stream") -> pure NoStream
        _ -> throwError $ "Expecting :inherit, :create-pipe, or :no-stream"
instance FromRad CreateProcess where
    fromRad x = case x of
        Dict _ d -> do
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


class ToRad a where
    toRad :: a -> Value'
    default toRad :: (HasEot a, ToRadG (Eot a)) => a -> Value'
    toRad = toRadG

instance ToRad () where
    toRad _ = Vec () Empty
instance ToRad Bool where
    toRad = Boolean ()
instance (ToRad a, ToRad b) => ToRad (a,b) where
    toRad (x,y) = Vec () $ toRad x :<| toRad y :<| Empty
instance (ToRad a) => ToRad (Maybe a) where
    toRad Nothing  = Keyword () (Ident "Nothing")
    toRad (Just x) = Vec () $ Keyword () (Ident "Just") :<| toRad x :<| Empty
instance ToRad Int where
    toRad = Number () . fromIntegral
instance ToRad Integer where
    toRad = Number () . fromIntegral
instance ToRad Scientific where
    toRad = Number () . toRational
instance ToRad Text where
    toRad = String ()
instance {-# OVERLAPPING #-} ToRad [Char] where
    toRad = String () . toS
instance ToRad ExitCode where
    toRad x = case x of
        ExitSuccess -> Keyword () (Ident "ok")
        ExitFailure c -> Vec () $ Seq.fromList [Keyword () (Ident "error"), toRad c]
instance ToRad Value' where
    toRad = identity
instance ToRad a => ToRad [a] where
    toRad xs = Vec () . Seq.fromList $ toRad <$> xs
instance ToRad a => ToRad (Map.Map Text a) where
    toRad xs = Dict () $ Map.mapKeys (String ()) $ toRad <$> xs
instance ToRad (Doc.Docd Value') where
    toRad (Doc.Docd d_ v) = Dict () $ Map.fromList $ ( Keyword () (Ident "val"), v) : case d_ of
      Just d  -> [ (Keyword () (Ident "doc"), toRad d) ]
      Nothing -> []
instance ToRad (Env Value') where
    toRad x = Dict () . Map.mapKeys (Atom ()) . Map.map toRad $ fromEnv x
instance ToRad (Bindings m) where
    toRad x = Dict () $ Map.fromList
        [ (Keyword () $ Ident "env", toRad $ bindingsEnv x)
        , (Keyword () $ Ident "refs", List () $ IntMap.elems (bindingsRefs x))
        ]
instance ToRad StdStream where
    toRad x = case x of
        Inherit    -> Keyword () $ Ident "inherit"
        CreatePipe -> Keyword () $ Ident "create-pipe"
        NoStream   -> Keyword () $ Ident "no-stream"
        _          -> panic "Cannot convert handle"
instance ToRad CmdSpec where
    toRad x = case x of
        ShellCommand comm ->
            let c = toRad comm
            in Vec () $ Seq.fromList [Keyword () (Ident "shell"), c]
        RawCommand f args ->
            let f' = toRad f
                args' = toRad args
            in Vec () $ Seq.fromList [Keyword () (Ident "raw"), f', args']

-- * Helpers

-- Loc is the source location of the application.
callFn :: (Monad m) => Value' -> [Value'] -> Lang m Value'
callFn f vs = case f of
  Lambda _ bnds body closure ->
      if length bnds /= length vs
          then throwErrorHere $ WrongNumberOfArgs "lambda" (length bnds)
                                                           (length vs)
          else do
              let mappings = GhcExts.fromList (Doc.noDocs $ zip bnds vs)
                  modEnv = mappings <> closure
              NonEmpty.last <$> withEnv (const modEnv)
                                        (traverse baseEval body)
  PrimFn _ i -> do
    fn <- lookupPrimop i
    fn vs
  _ -> throwErrorHere . TypeError $ "Trying to call a non-function: " <> show f

-- | Infix evaluation of application (of functions or special forms)
infixr 1 $$
($$)
    :: (Monad m)
    => Value'
    -> [Value']
    -> Lang m Value'
f $$ vs = case f of
    Atom _ i ->
      case Map.lookup i specialForms of
        Just form -> form vs
        Nothing   -> fnApp
    _ -> fnApp
  where
    fnApp = do
      f' <- baseEval f
      vs' <- traverse baseEval vs
      callFn f' vs'

nil :: (Extra "list" x ~ ()) => Value x
nil = List () []

-- quote :: Value x -> Value x
-- quote v = List [Atom (Ident "quote"), v]


-- list :: [Value x] -> Value x
-- list vs = List () (Atom () (Ident "list") : vs)

kwLookup :: Text -> Map Data x -> Maybe x
kwLookup key = Map.lookup (Keyword () $ Ident key)

(??) :: MonadError e m => Maybe a -> e -> m a
a ?? n = n `note` a

hoistEither :: MonadError e m => Either e a -> m a
hoistEither = hoistEitherWith identity

hoistEitherWith :: MonadError e' m => (e -> e') -> Either e a -> m a
hoistEitherWith f (Left e)  = throwError (f e)
hoistEitherWith _ (Right x) = pure x

-- | Some forms/functions expect an even number or arguments.
evenArgs :: MonadError (LangError (Value x)) m => Text -> [b] -> m [(b, b)]
evenArgs name = \case
    [] -> pure []
    [_] -> throwErrorHere . OtherError $ name <> ": expects an even number of arguments"
    x:y:xs -> do
        ps <- evenArgs name xs
        pure ((x,y):ps)

-- * Generic encoding/decoding of Radicle values.

toRadG :: forall a. (HasEot a, ToRadG (Eot a)) => a -> Value'
toRadG x = toRadConss (constructors (datatype (Proxy :: Proxy a))) (toEot x)

class ToRadG a where
  toRadConss :: [Constructor] -> a -> Value'

instance (ToRadFields a, ToRadG b) => ToRadG (Either a b) where
  toRadConss (Constructor name fieldMeta : _) (Left fields) =
    case fieldMeta of
      Selectors names ->
        radCons (toS name) . pure . Dict () . Map.fromList $
          zip (Keyword () . Ident . toS <$> names) (toRadFields fields)
      NoSelectors _ -> radCons (toS name) (toRadFields fields)
      NoFields -> radCons (toS name) []
  toRadConss (_ : r) (Right next) = toRadConss r next
  toRadConss [] _ = panic "impossible"

radCons :: Text -> [Value'] -> Value'
radCons name args = case args of
    [] -> consKw
    _  -> Vec () ( consKw :<| Seq.fromList args )
  where
    consKw = Keyword () (Ident (Identifier.keywordWord name))

instance ToRadG Void where
  toRadConss _ = absurd

class ToRadFields a where
  toRadFields :: a -> [Value']

instance (ToRad a, ToRadFields as) => ToRadFields (a, as) where
  toRadFields (x, xs) = toRad x : toRadFields xs

instance ToRadFields () where
  toRadFields () = []

fromRadG :: forall a. (HasEot a, FromRadG (Eot a)) => Value' -> Either Text a
fromRadG v = do
  (name, args) <- isRadCons v ?? gDecodeErr "expecting constructor"
  fromEot <$> fromRadConss (constructors (datatype (Proxy :: Proxy a))) name args

class FromRadG a where
  fromRadConss :: [Constructor] -> Text -> [Value'] -> Either Text a

isRadCons :: Value' -> Maybe (Text, [Value'])
isRadCons (Keyword  _ (Ident name))                 = pure (name, [])
isRadCons (Vec _ (Keyword _ (Ident name) :<| args)) = pure (name, toList args)
isRadCons _                                         = Nothing

gDecodeErr :: Text -> Text
gDecodeErr e = "Couldn't generically decode radicle value: " <> e

instance (FromRadFields a, FromRadG b) => FromRadG (Either a b) where
  fromRadConss (Constructor name fieldMeta : r) name' args = do
    if Identifier.keywordWord (toS name) /= name'
      then Right <$> fromRadConss r name' args
      else Left <$> fromRadFields fieldMeta args
  fromRadConss [] _ _ = panic "impossible"

instance FromRadG Void where
  fromRadConss _ name _ = Left (gDecodeErr "unknown constructor '" <> name <> "'")

class FromRadFields a where
  fromRadFields :: Fields -> [Value'] -> Either Text a

instance (FromRad a, FromRadFields as) => FromRadFields (a, as) where
  fromRadFields fields args = case fields of
    NoSelectors _ -> case args of
      v:vs -> do
        x <- fromRad v
        xs <- fromRadFields fields vs
        pure (x, xs)
      _ -> panic "impossible"
    Selectors (n:names) -> case args of
      [Dict _ d] -> do
        xv <- kwLookup (toS n) d ?? gDecodeErr ("missing field '" <> toS n <> "'")
        x <- fromRad xv
        xs <- fromRadFields (Selectors names) args
        pure (x, xs)
      _ -> Left . gDecodeErr $ "expecting a dict"
    _ -> panic "impossible"

instance FromRadFields () where
  fromRadFields _ _ = pure ()
