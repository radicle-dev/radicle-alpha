module Radicle.Internal.Eval
    ( transact
    , baseEval
    , callFn
    , ($$)
    ) where

import           Protolude hiding (Constructor, Handle, TypeError, (<>))

import           Data.Copointed (copoint)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import qualified GHC.Exts as GhcExts

import           Radicle.Internal.Core
import qualified Radicle.Internal.Doc as Doc
import           Radicle.Internal.Identifier
                 ( Ident(..)
                 , Naked(..)
                 , pattern NakedN
                 , pattern NakedT
                 , Unnamespaced(..)
                 )
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
    nss <- gets bindingsNamespaces
    tx' <- lookupInNamespace True nss (Naked "toplevel") (NakedU (Naked "tx"))
    let tx = copoint tx'
    logValPos tx $ do
        expr' <- callFn tx [expr]
        baseEval expr'

specialForms :: forall m. (Monad m) => Map Naked ([Value] -> Lang m Value)
specialForms = Map.fromList $ first Naked <$>
  [ ( "fn"
    , \case
          args : b : bs -> do
            e <- gets bindingsEnv
            cns <- gets bindingsCurrentNamespace
            let toLambda argNames = pure (Lambda argNames (b :| bs) e cns)
            case args of
              Vec atoms_ -> do
                atoms <- traverse isNaked (toList atoms_) ?? toLangError (SpecialForm "fn" "One of the arguments was not an atom")
                toLambda (PosArgs atoms)
              Atom (Unnamespaced (NakedU name)) -> do
                toLambda (VarArgs name)
              _ -> throwErrorHere $ SpecialForm "fn" "Function parameters must be one atom or a vector of atoms"
          _ -> throwErrorHere $ SpecialForm "fn" "Function needs parameters (one atom or a vector of atoms) and a body"
      )
  , ( "ns", \case
        [a@(Atom (NakedN i))] -> ns a i Nothing
        [a@(Atom (NakedN i)), String doc] -> ns a i (Just doc)
        _ -> throwErrorHere $ SpecialForm "ns" "The `ns` form expects a (naked) symbol and optionally a docstring."
    )
  , ( "require"
    , \case
        [Atom (NakedN i), e] -> require i e Nothing
        [Atom (NakedN i), e, Atom (Unnamespaced (NakedU q))] -> require i e (Just q)
        _ -> throwErrorHere $ OtherError "require needs a namespace identifier and a vector of symbols to import."
    )
  , ("quote", \case
          [v] -> pure v
          xs  -> throwErrorHere $ WrongNumberOfArgs "quote" 1 (length xs))
  , ( "macro"
    , \case
        [val] -> Macro <$> baseEval val
        _ -> throwErrorHere $ SpecialForm "macro" "Takes a single argument, which should evaluate to a function (which transforms syntax)."
    )
  , ( "def"
    , defPrim Public
    )
  , ( "defp"
    , defPrim Private
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
                     if thrownLabel == label || label == NakedT "any"
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
    require i e q_ = do
      syms__ <- baseEval e
      case syms__ of
        Vec syms_ -> case traverse isNaked syms_ of
          Just syms -> do
            let binds = Map.fromList [(qualer s, There i (NakedU s)) | s <- toList syms ]
            _ <- modifyCurrentNamespace (binds <>)
            pure (Keyword (NakedT "ok"))
          Nothing -> throwErrorHere $ OtherError "One of the items in the vector was not a (unnamespaced) symbol."
        _ -> throwErrorHere $ OtherError "require needs a vector of symbols to import."
      where
        qualer :: Naked -> Unnamespaced
        qualer = maybe NakedU (flip Qualified) q_
    ns a i d_ = do
      nss <- gets bindingsNamespaces
      let nss' = Map.alter (Just . maybe (Doc.Docd d_ mempty) (Doc.redoc d_)) i nss
      _ <- modify $ \s -> s { bindingsCurrentNamespace = i
                            , bindingsNamespaces = nss' }
      pure a
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
        matchPat <- lookupAtom (NakedT "match-pat")
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
        isBind (Atom (NakedN x), v) = pure (x, Doc.Docd Nothing v)
        isBind _ = throwErrorHere $ PatternMatchError (BadBindings pat)

    addBinds e = modify (\s -> s { bindingsEnv = e <> bindingsEnv s })

    defPrim vis = \case
        [Atom (NakedN name), val] -> def (NakedU name) vis Nothing val
        [_, _] -> throwErrorHere $ OtherError "def expects a naked symbols for the first arg"
        [Atom (NakedN name), String d, val] -> def (NakedU name) vis (Just d) val
        xs -> throwErrorHere $ WrongNumberOfArgs "def" 2 (length xs)

    def name vis doc_ val = do
      val' <- baseEval val
      defineAtomInNs name vis doc_ val'
      pure nil

-- | Call a lambda or primitive function @f@ with @arguments@. @f@ and
-- @argumenst@ are not evaluated.
callFn :: forall m. Monad m => Value -> [Value] -> Lang m Value
callFn f arguments = case f of
    Lambda argNames body closure ns -> do
        args <- argumentBindings argNames
        evalManyWithEnv ns (args <> closure) body
    PrimFn i -> do
        fn <- lookupPrimop i
        fn arguments
    _ -> throwErrorHere $ NonFunctionCalled f
  where
    evalManyWithEnv :: Naked -> Env Value -> NonEmpty Value -> Lang m Value
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
      where
        toArgs = pure . GhcExts.fromList . Doc.noDocs

-- | Process special forms or call function. If @f@ is an atom that
-- indicates a special form that special form is processed. Otherwise
-- @f@ and @vs@ are evaluated in 'callFn' is called.
infixr 1 $$
($$) :: Monad m => Value -> [Value] -> Lang m Value
f $$ vs = case f of
    Atom (NakedN i) ->
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
