module Radicle.Internal.Primops
  ( pureEnv
  , purePrimops
  , evalArgs
  , evalOneArg
  , readValue
  ) where

import           Protolude hiding (TypeError, toList)

import qualified Data.Aeson as Aeson
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Scientific (Scientific, floatingOrInteger)
import           GHC.Exts (IsList(..))

import           Radicle.Internal.Core
import           Radicle.Internal.Crypto
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: (Monad m) => Bindings (Primops m)
pureEnv = Bindings e purePrimops r 1
  where
    e = fromList [ (toIdent "eval", Primop $ toIdent "base-eval")
                 , (toIdent "_doc-ref", Ref $ Reference 0)
                 ]
    r = fromList [ (0, Dict mempty) ]

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable via 'define'.
purePrimops :: forall m. (Monad m) => Primops m
purePrimops = Primops $ fromList $ first Ident <$>
    [ ( "fn"
      , \case
          args : b : bs ->
            case args of
              Vec atoms_ -> do
                atoms <- traverse isAtom (toList atoms_) ?? toLangError (TypeError "fn: expecting a list of symbols")
                e <- gets bindingsEnv
                pure (Lambda atoms (b :| bs) e)
              _ -> throwErrorHere $ OtherError "fn: first argument must be a vector of argument symbols, and then at least one form for the body"
          xs -> throwErrorHere $ WrongNumberOfArgs "fn" 2 (length xs) -- TODO: technically "at least 2"
      )
    , ( "base-eval"
      , evalArgs $ \case
          [expr, st] -> case (fromRad st :: Either Text (Bindings ())) of
              Left e -> throwErrorHere $ OtherError e
              Right st' -> do
                prims <- gets bindingsPrimops
                withBindings (const $ fmap (const prims) st') $ do
                  val <- baseEval expr
                  st'' <- get
                  pure $ List [val, toRad st'']
          xs -> throwErrorHere $ WrongNumberOfArgs "base-eval" 2 (length xs)
      )
    , ( "pure-env"
      , \case
          [] -> pure $ toRad (pureEnv :: Bindings (Primops m))
          xs -> throwErrorHere $ WrongNumberOfArgs "pure-env" 0 (length xs)
      )
    , ("apply", evalArgs $ \case
          [fn, List args] -> eval . List $ fn:args
          [_, _]          -> throwErrorHere $ TypeError "apply: expecting list as second arg"
          xs -> throwErrorHere $ WrongNumberOfArgs "apply" 2 (length xs))
    , ( "read"
      , evalOneArg "read" $ \case
          String s -> readValue s
          _ -> throwErrorHere $ TypeError "read: expects string"
      )
    , ("get-current-env", \case
          [] -> toRad <$> get
          xs -> throwErrorHere $ WrongNumberOfArgs "get-current-env" 0 (length xs))
    , ("list", evalArgs $ \args -> pure $ List args)
    , ("dict", evalArgs $ (Dict . foldr (uncurry Map.insert) mempty <$>)
                        . evenArgs "dict")
    , ("quote", \args -> case args of
          [v] -> pure v
          xs  -> throwErrorHere $ WrongNumberOfArgs "quote" 1 (length xs))
    , ("def", \args -> case args of
          [Atom name, val] -> do
              val' <- baseEval val
              defineAtom name val'
              pure nil
          [_, _]           -> throwErrorHere $ OtherError "def expects atom for first arg"
          xs               -> throwErrorHere $ WrongNumberOfArgs "def" 2 (length xs))
    , ( "def-rec"
      , \case
          [Atom name, val] -> do
            val' <- baseEval val
            case val' of
                Lambda is b e -> do
                    let v = Lambda is b (Env . Map.insert name v . fromEnv $ e)
                    defineAtom name v
                    pure nil
                _ -> throwErrorHere $ OtherError "def-rec can only be used to define functions"
          [_, _]           -> throwErrorHere $ OtherError "def-rec expects atom for first arg"
          xs               -> throwErrorHere $ WrongNumberOfArgs "def-rec" 2 (length xs)
      )
    , ("do", evalArgs $ pure . lastDef nil)
    , ("catch", \args -> case args of
          [l, form, handler] -> do
              mlabel <- baseEval l
              case mlabel of
                  -- TODO reify stack
                  Atom label -> baseEval form `catchError` \(LangError _stack e) -> do
                     (thrownLabel, thrownValue) <- errorDataToValue e
                     if thrownLabel == label || label == Ident "any"
                         then handler $$ [thrownValue]
                         else baseEval form
                  _ -> throwErrorHere $ TypeError "catch: first argument must be atom"
          xs -> throwErrorHere $ WrongNumberOfArgs "catch" 3 (length xs))
    , ("throw", evalArgs $ \args -> case args of
          [Atom label, exc] -> throwErrorHere $ ThrownError label exc
          [_, _]            -> throwErrorHere $ TypeError "throw: first argument must be atom"
          xs                -> throwErrorHere $ WrongNumberOfArgs "throw" 2 (length xs))
    , ("eq?", evalArgs $ \args -> case args of
          [a, b] -> pure $ Boolean (a == b)
          xs     -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("cons", evalArgs $ \args -> case args of
          [x, List xs] -> pure $ List (x:xs)
          [_, _]       -> throwErrorHere $ TypeError "cons: second argument must be list"
          xs           -> throwErrorHere $ WrongNumberOfArgs "cons" 2 (length xs))
    , ("head", evalOneArg "head" $ \case
          List (x:_) -> pure x
          List []    -> throwErrorHere $ OtherError "head: empty list"
          _          -> throwErrorHere $ TypeError "head: expects list argument")
    , ("tail", evalOneArg "tail" $ \case
          List (_:xs) -> pure $ List xs
          List []     -> throwErrorHere $ OtherError "tail: empty list"
          _           -> throwErrorHere $ TypeError "tail: expects list argument")
    , ( "nth"
      , evalArgs $ \case
          [Number n, vs] -> case floatingOrInteger n of
            Left (_ :: Double) -> throwErrorHere $ OtherError "nth: first argument was not an integer"
            Right i -> do
              xs <- hoistEitherWith (const (toLangError . OtherError $ "nth: first argument must be sequential")) $ fromRad vs
              case xs `atMay` i of
                Just x  -> pure x
                Nothing -> throwErrorHere $ OtherError "nth: index out of bounds"
          [_,_] -> throwErrorHere $ TypeError "nth: expects a integer and a list"
          xs -> throwErrorHere $ WrongNumberOfArgs "nth" 2 (length xs)
      )
    , ("lookup", evalArgs $ \args -> case args of
          [a, Dict m] -> pure $ case Map.lookup a m of
              Just v  -> v
              -- Probably an exception is better, but that seems cruel
              -- when you have no exception handling facilities.
              Nothing -> nil
          [_, _]      -> throwErrorHere $ TypeError "lookup: second argument must be map"
          xs -> throwErrorHere $ WrongNumberOfArgs "lookup" 2 (length xs))
    , ("string-append", evalArgs $ \args ->
          let fromStr (String s) = Just s
              fromStr _          = Nothing
              ss = fromStr <$> args
          in if all isJust ss
              then pure . String . mconcat $ catMaybes ss
              else throwErrorHere $ TypeError "string-append: non-string argument")
    , ("insert", evalArgs $ \args -> case args of
          [k, v, Dict m] -> pure . Dict $ Map.insert k v m
          [_, _, _]                -> throwErrorHere
                                    $ TypeError "insert: third argument must be a dict"
          xs -> throwErrorHere $ WrongNumberOfArgs "insert" 3 (length xs))
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
          [_, _]               -> throwErrorHere $ TypeError "<: expecting number"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "<" 2 (length xs))
    , (">", evalArgs $ \args -> case args of
          [Number x, Number y] -> pure $ Boolean (x > y)
          [_, _]               -> throwErrorHere $ TypeError ">: expecting number"
          xs                   -> throwErrorHere $ WrongNumberOfArgs ">" 2 (length xs))
    , ("foldl", evalArgs $ \args -> case args of
          [fn, init', List ls] -> foldlM (\b a -> callFn fn [b, a]) init' ls
          [_, _, _]            -> throwErrorHere
                                $ TypeError "foldl: third argument should be a list"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "foldl" 3 (length xs))
    , ("foldr", evalArgs $ \args -> case args of
          [fn, init', List ls] -> foldrM (\b a -> callFn fn [b, a]) init' ls
          [_, _, _]            -> throwErrorHere
                                $ TypeError "foldr: third argument should be a list"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "foldr" 3 (length xs))
    , ("map", evalArgs $ \args -> case args of
          [fn, List ls] -> List <$> traverse (callFn fn) (pure <$> ls)
          [_, _]        -> throwErrorHere $ TypeError "map: second argument should be a list"
          xs            -> throwErrorHere $ WrongNumberOfArgs "map" 3 (length xs))
    , ("keyword?", evalOneArg "keyword?" $ \case
          Keyword _ -> pure tt
          _         -> pure ff)
    , ("atom?", evalOneArg "atom?" $ \case
                  Atom _ -> pure tt
                  _      -> pure ff)
    , ("list?", evalOneArg "list?" $ \case
                  List _ -> pure tt
                  _      -> pure ff)
    , ("dict?", evalOneArg "dict?" $ \case
                  Dict _ -> pure tt
                  _      -> pure ff)
    , ( "type"
      , let kw = pure . Keyword . Ident
        in evalOneArg "type" $ \case
             Atom _ -> kw "atom"
             Keyword _ -> kw "keyword"
             String _ -> kw "string"
             Number _ -> kw "number"
             Boolean _ -> kw "boolean"
             List _ -> kw "list"
             Primop _ -> kw "primop"
             Dict _ -> kw "dict"
             Ref _ -> kw "ref"
             Lambda{} -> kw "function"
      )
    , ("string?", evalOneArg "string?" $ \case
          String _ -> pure tt
          _        -> pure ff)
    , ("boolean?", evalOneArg "boolean?" $ \case
          Boolean _ -> pure tt
          _         -> pure ff)
    , ("number?", evalOneArg "number?" $ \case
          Number _ -> pure tt
          _        -> pure ff)
    , ("member?", evalArgs $ \args -> case args of
          [x, List xs] -> pure . Boolean $ elem x xs
          [_, _]       -> throwErrorHere
                        $ TypeError "member?: second argument must be list"
          xs           -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("if", \args -> case args of
          [condition, t, f] -> do
            b <- baseEval condition
            -- I hate this as much as everyone that might ever read Haskell, but
            -- in Lisps a lot of things that one might object to are True...
            if b == ff then baseEval f else baseEval t
          xs -> throwErrorHere $ WrongNumberOfArgs "if" 3 (length xs))
    , ( "cond", (cond =<<) . evenArgs "cond" )
    , ("ref", evalOneArg "ref" newRef)
    , ("read-ref", evalOneArg "read-ref" $ \case
          Ref ref -> readRef ref
          _       -> throwErrorHere $ TypeError "read-ref: argument must be a ref")
    , ("write-ref", evalArgs $ \args -> case args of
          [Ref (Reference x), v] -> do
              st <- get
              put $ st { bindingsRefs = IntMap.insert x v $ bindingsRefs st }
              pure nil
          [_, _]                 -> throwErrorHere
                                  $ TypeError "write-ref: first argument must be a ref"
          xs                     -> throwErrorHere
                                  $ WrongNumberOfArgs "write-ref" 2 (length xs))
    , ("show", evalOneArg "show" (pure . String . renderPrettyDef))
    , ( "seq"
      , evalOneArg "seq" $
          \case
            x@(List _) -> pure x
            x@(Vec _) -> pure x
            Dict kvs -> pure $ List [List [k, v] | (k,v) <- Map.toList kvs ]
            _ -> throwErrorHere $ TypeError "seq: can only be used on a list, vector or dictionary"
      )
    , ( "to-json"
      , evalOneArg "to-json" $ \v -> String . toS . Aeson.encode <$>
          maybeJson v ?? toLangError (OtherError "Could not serialise value to JSON")
      )
    , ( "default-ecc-curve",
        evalArgs $ \case
          [] -> pure $ toRad defaultCurve
          xs -> throwErrorHere $ WrongNumberOfArgs "default-ecc-curve" 0 (length xs)
      )
    , ( "verify-signature"
      , evalArgs $ \case
          [keyv, sigv, String msg] -> do
            key <- hoistEither . first (toLangError . OtherError) $ fromRad keyv
            sig <- hoistEither . first (toLangError . OtherError) $ fromRad sigv
            pure . Boolean $ verifySignature key sig msg
          [_, _, _] -> throwErrorHere $ OtherError "verify-signature: message must be a string"
          xs -> throwErrorHere $ WrongNumberOfArgs "verify-signature" 3 (length xs)
      )
    ]
  where

    cond = \case
      [] -> pure nil
      (c,e):ps -> do
        b <- baseEval c
        if b /= ff
          then baseEval e
          else cond ps

    -- | Some forms/functions expect an even number or arguments.
    evenArgs name = \case
      [] -> pure []
      [_] -> throwErrorHere . OtherError $ name <> ": expects an even number of arguments"
      x:y:xs -> do
        ps <- evenArgs name xs
        pure ((x,y):ps)

    tt = Boolean True
    ff = Boolean False

    numBinop :: (Scientific -> Scientific -> Scientific)
             -> Text
             -> (Text, [Value] -> Lang m Value)
    numBinop fn name = (name, evalArgs $ \args -> case args of
        Number x:x':xs -> foldM go (Number x) (x':xs)
          where
            go (Number a) (Number b) = pure . Number $ fn a b
            go _ _ = throwErrorHere . TypeError
                   $ name <> ": expecting number"
        [Number _] -> throwErrorHere
                    $ OtherError $ name <> ": expects at least 2 arguments"
        _ -> throwErrorHere $ TypeError $ name <> ": expecting number")

-- * Helpers

-- | Many primops evaluate their arguments just as normal functions do.
evalArgs :: Monad m => ([Value] -> Lang m Value) -> [Value] -> Lang m Value
evalArgs f args = traverse baseEval args >>= f

-- Many primops evaluate a single argument.
evalOneArg :: Monad m => Text -> (Value -> Lang m Value) -> [Value] -> Lang m Value
evalOneArg fname f = evalArgs $ \case
  [x] -> f x
  xs -> throwErrorHere $ WrongNumberOfArgs fname 1 (length xs)

readValue
    :: (MonadError (LangError Value) m, MonadState (Bindings (Primops n)) m)
    => Text
    -> m Value
readValue s = do
    allPrims <- gets (getPrimops . bindingsPrimops)
    let p = parse "[read-primop]" s (Map.keys allPrims)
    case p of
      Right v -> pure v
      Left e  -> throwErrorHere $ ThrownError (Ident "parse-error") (String e)
