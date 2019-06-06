module Radicle.Internal.Eval
    ( transact
    , baseEval
    , callFn
    , ($$)
    , createModule
    ) where

import           Protolude hiding (Constructor, Handle, TypeError, (<>))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified GHC.Exts as GhcExts

import           Radicle.Internal.Core
import qualified Radicle.Internal.Doc as Doc
import           Radicle.Internal.Identifier (Ident(..))
import           Radicle.Internal.Orphans ()

-- | Basic evaluation.
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


-- |
-- A transaction happens as follows:
-- - First @tx@ is resolved, this is expected to be invocable.
-- - It is invoked on the input expression.
-- - The result of this is evaluated normally.
transact :: Monad m => Value -> Lang m Value
transact expr = do
    tx <- lookupAtom (Ident "tx")
    logValPos tx $ do
        expr' <- callFn tx [expr]
        baseEval expr'

specialForms :: forall m. (Monad m) => Map Ident ([Value] -> Lang m Value)
specialForms = Map.fromList $ first Ident <$>
  [ ( "fn"
    , \case
          args : b : bs -> do
            e <- gets bindingsEnv
            ns <- gets bindingsCurrentNamespace
            let toLambda argNames = pure (Lambda argNames (b :| bs) e ns)
            case args of
              Vec atoms_ -> do
                atoms <- traverse isAtom (toList atoms_) ?? toLangError (SpecialForm "fn" "One of the arguments was not an atom")
                toLambda (PosArgs atoms)
              Atom name -> do
                toLambda (VarArgs name)
              _ -> throwErrorHere $ SpecialForm "fn" "Function parameters must be one atom or a vector of atoms"
          _ -> throwErrorHere $ SpecialForm "fn" "Function needs parameters (one atom or a vector of atoms) and a body"
      )
  , ( "module", createModule )
  , ( "ns", \case
        [a@(Atom i)] -> do
          -- TODO(james): add optional arg to ns to add documentation to namespace.
          nss <- gets bindingsNamespaces
          let nss' = if Map.member i (fromEnv nss)
                     then nss
                     else addToEnv i Nothing mempty nss
          _ <- modify $ \s -> s { bindingsCurrentNamespace = i
                                , bindingsNamespaces = nss' }
          pure a
        _ -> throwErrorHere $ SpecialForm "ns" "The `ns` form expects a symbol."
    )
  , ("quote", \case
          [v] -> pure v
          xs  -> throwErrorHere $ WrongNumberOfArgs "quote" 1 (length xs))
  , ( "macro"
    , \case
        [val] -> Macro <$> baseEval val
        _ -> throwErrorHere $ SpecialForm "macro" "Takes a single argument, which should evaluate to a function (which transforms syntax)."
    )
  , ("def", \case
          [Atom name, val] -> def name Nothing val
          [_, _]           -> throwErrorHere $ OtherError "def expects atom for first arg"
          [Atom name, String d, val] -> def name (Just d) val
          xs -> throwErrorHere $ WrongNumberOfArgs "def" 2 (length xs))
    , ( "def-rec"
      , \case
          [Atom name, val] -> def name Nothing val
          [_, _]           -> throwErrorHere $ OtherError "def-rec expects atom for first arg"
          [Atom name, String d, val] -> def name (Just d) val
          xs               -> throwErrorHere $ WrongNumberOfArgs "def-rec" 2 (length xs)
      )
    , ("do", (lastDef nil <$>) . traverse baseEval)
    , ("catch", \case
          [l, form, handler] -> do
              mlabel <- baseEval l
              s <- get
              case mlabel of
                  -- TODO reify stack
                  Atom label -> baseEval form `catchError` \err@(LangError _stack e) -> do
                     (thrownLabel, thrownValue) <- errorDataToValue e
                     if thrownLabel == label || label == Ident "any"
                         then do
                            put s
                            handlerclo <- baseEval handler
                            callFn handlerclo [thrownValue]
                         else throwError err
                  _ -> throwErrorHere $ SpecialForm "catch" "first argument must be atom"
          xs -> throwErrorHere $ WrongNumberOfArgs "catch" 3 (length xs))
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
        matchPat <- lookupAtom (Ident "match-pat")
        goMatches matchPat v' cs
      _ -> throwErrorHere $ PatternMatchError NoValue

    goMatches _ _ [] = throwErrorHere (PatternMatchError NoMatch)
    goMatches matchPat v ((m, body):cases) = do
      patFn <- baseEval m
      res <- callFn matchPat [patFn, v]
      let res_ = fromRad res
      case res_ of
        Right (Just (Dict binds)) -> do
          b <- bindsToEnv m binds
          addBinds b *> baseEval body
        Right Nothing -> goMatches matchPat v cases
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

    -- defRec name doc_ val = do
    --   val' <- baseEval val
    --   case val' of
    --     Lambda is b e -> do
    --       defineAtom name doc_ $ LambdaRec name is b e
    --       pure nil
    --     LambdaRec{} -> throwErrorHere $
    --         OtherError "'def-rec' cannot be used to alias functions. Use 'def' instead"
    --     _ -> throwErrorHere $ OtherError "'def-rec' can only be used to define functions"

data ModuleMeta = ModuleMeta
  { name    :: Ident
  , exports :: [Ident]
  , doc     :: Text
  }

-- | Given a list of forms, the first of which should be module declaration,
-- runs the rest of the forms in a new scope, and then defs the module value
-- according to the name in the declaration.
createModule :: forall m. Monad m => [Value] -> Lang m Value
createModule = \case
    (m : forms) -> do
      m' <- baseEval m >>= meta
      -- TODO: The @transact@ in the following line is a bit of a wart, but
      -- modules are thought of as a sequence of "top-level" s-expressions.
      e <- withEnv identity identity $ traverse_ transact forms *> gets bindingsEnv
      let exportsSet = Set.fromList (exports m')
      let undefinedExports = Set.difference exportsSet (Map.keysSet (fromEnv e))
      env <- if null undefinedExports
               then pure . VEnv . Env $ Map.restrictKeys (fromEnv e) exportsSet
               else throwErrorHere (ModuleError (UndefinedExports (name m') (Set.toList undefinedExports)))
      let modu = Dict $ Map.fromList
                  [ (Keyword (Ident "module"), Atom (name m'))
                  , (Keyword (Ident "env"), env)
                  , (Keyword (Ident "exports"), Vec (Seq.fromList (Atom <$> exports m')))
                  ]
      defineAtom (name m') (Just (doc m')) modu
      pure modu
    _ ->  throwErrorHere (ModuleError MissingDeclaration)
  where
    meta v@(Dict d) = do
      name <- modLookup v "module" d "missing `:module` key"
      doc  <- modLookup v "doc" d "missing `:doc` key"
      exports <- modLookup v "exports" d "Missing `:exports` key"
      case (name, doc, exports) of
        (Atom n, String ds, Vec es) -> do
          is <- traverse isAtom es ?? toLangError (ModuleError (InvalidDeclaration "`:exports` must be a vector of atoms" v))
          pure $ ModuleMeta n (toList is) ds
        _ -> throwErrorHere (ModuleError (InvalidDeclaration "`:module` must be an atom, `:doc` must be a string and `:exports` must be a vector" v))
    meta v = throwErrorHere (ModuleError (InvalidDeclaration "must be dict" v))
    modLookup v k d e = kwLookup k d ?? toLangError (ModuleError (InvalidDeclaration e v))


-- | Call a lambda or primitive function @f@ with @arguments@. @f@ and
-- @argumenst@ are not evaluated.
callFn :: forall m. Monad m => Value -> [Value] -> Lang m Value
callFn f arguments = case f of
    Lambda argNames body closure ns -> do
        args <- argumentBindings argNames
        evalManyWithEnv ns (args <> closure) body
    -- LambdaRec self argNames body closure -> do
    --     args <- argumentBindings argNames
    --     let selfBinding = GhcExts.fromList (Doc.noDocs [(self, f)])
    --     evalManyWithEnv (args <> selfBinding <> closure) body
    PrimFn i -> do
        fn <- lookupPrimop i
        fn arguments
    _ -> throwErrorHere $ NonFunctionCalled f
  where
    evalManyWithEnv :: Ident -> Env Value -> NonEmpty Value -> Lang m Value
    evalManyWithEnv ns env exprs =
      NonEmpty.last <$> withEnv (const ns) (const env) (traverse baseEval exprs)
    argumentBindings :: LambdaArgs -> Lang m (Env Value)
    argumentBindings argNames = case argNames of
      PosArgs names ->
        if length names /= length arguments
        then throwErrorHere $ WrongNumberOfArgs "lambda" (length names) (length arguments)
        else toArgs $ zip names arguments
      VarArgs name ->
        toArgs [(name, Vec $ Seq.fromList arguments)]
      where toArgs = pure . GhcExts.fromList . Doc.noDocs

-- | Process special forms or call function. If @f@ is an atom that
-- indicates a special form that special form is processed. Otherwise
-- @f@ and @vs@ are evaluated in 'callFn' is called.
infixr 1 $$
($$) :: Monad m => Value -> [Value] -> Lang m Value
f $$ vs = case f of
    Atom i ->
      case Map.lookup i specialForms of
        Just form -> form vs
        Nothing   -> appFnOrMacro
    _ -> appFnOrMacro
  where
    appFnOrMacro = do
      f' <- baseEval f
      case f' of
        Macro g -> do
          e <- callFn g vs
          baseEval e
        _ -> do
          vs' <- traverse baseEval vs
          callFn f' vs'
