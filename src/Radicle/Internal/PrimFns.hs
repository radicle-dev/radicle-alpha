module Radicle.Internal.PrimFns
  ( pureEnv
  , purePrimFns
  , oneArg
  , evenArgs
  , readValue
  , primFnsEnv
  ) where

import           Protolude hiding (TypeError)

import qualified Data.Aeson as Aeson
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Scientific (Scientific, floatingOrInteger)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           GHC.Exts (IsList(..))

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Crypto
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: forall m. (Monad m) => Bindings (PrimFns m)
pureEnv = Bindings e purePrimFns r 1
  where
    e = fromList [ (unsafeToIdent "eval", PrimFn $ unsafeToIdent "base-eval")
                 , (unsafeToIdent "_doc-ref", Ref $ Reference 0)
                 ]
        <> primFnsEnv (purePrimFns :: PrimFns m)
    r = fromList [ (0, Dict mempty) ]

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable via 'define'.
purePrimFns :: forall m. (Monad m) => PrimFns m
purePrimFns = PrimFns $ fromList $ first Ident <$>
    [ ( "base-eval"
      , \case
          [expr, st] -> case (fromRad st :: Either Text (Bindings ())) of
              Left e -> throwErrorHere $ OtherError e
              Right st' -> do
                prims <- gets bindingsPrimFns
                withBindings (const $ fmap (const prims) st') $ do
                  val <- baseEval expr
                  st'' <- get
                  pure $ List [val, toRad st'']
          xs -> throwErrorHere $ WrongNumberOfArgs "base-eval" 2 (length xs)
      )
    , ( "pure-env"
      , \case
          [] -> pure $ toRad (pureEnv :: Bindings (PrimFns m))
          xs -> throwErrorHere $ WrongNumberOfArgs "pure-env" 0 (length xs)
      )
    , ("apply", \case
          [fn, List args] -> callFn fn args
          [_, _]          -> throwErrorHere $ TypeError "apply: expecting list as second arg"
          xs -> throwErrorHere $ WrongNumberOfArgs "apply" 2 (length xs))
    , ( "read"
      , oneArg "read" $ \case
          String s -> readValue s
          _ -> throwErrorHere $ TypeError "read: expects string"
      )
    , ("get-current-env", \case
          [] -> toRad <$> get
          xs -> throwErrorHere $ WrongNumberOfArgs "get-current-env" 0 (length xs))
    , ( "set-current-env"
      , oneArg "set-current-env" $ \x -> do
          e' :: Bindings () <- fromRadOtherErr x
          e <- get
          put e { bindingsEnv = bindingsEnv e'
                , bindingsNextRef = bindingsNextRef e'
                , bindingsRefs = bindingsRefs e'
                }
          pure ok
      )
    , ("list", pure . List)
    , ("dict", (Dict . foldr (uncurry Map.insert) mempty <$>)
                        . evenArgs "dict")
    , ("throw", \case
          [Atom label, exc] -> throwErrorHere $ ThrownError label exc
          [_, _]            -> throwErrorHere $ TypeError "throw: first argument must be atom"
          xs                -> throwErrorHere $ WrongNumberOfArgs "throw" 2 (length xs))
    , ("eq?", \case
          [a, b] -> pure $ Boolean (a == b)
          xs     -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))

    -- Maybe
    , ( "maybe"
      , \case
          [Keyword (Ident "Nothing"), nothing, _] -> pure nothing
          [Vec (Keyword (Ident "Just") :<| v :<| Empty), _, just] -> callFn just [v]
          [x, _, _] -> throwErrorHere $ TypeError $ "maybe: first argument should be a Maybe: " <> renderCompactPretty x
          xs -> throwErrorHere $ WrongNumberOfArgs "maybe" 3 (length xs)
      )

    -- Vectors
    , ( "<>"
      , twoArg "<>" $ \case
          (Vec xs, Vec ys) -> pure $ Vec (xs Seq.>< ys)
          _ -> throwErrorHere $ TypeError "<>: both arguments must be vectors"
      )
    , ( "add-left"
      , twoArg "add-left" $ \case
          (x, Vec xs) -> pure $ Vec (x :<| xs)
          _ -> throwErrorHere $ TypeError "add-left: second argument must be a vector"
      )
    , ( "add-right"
      , twoArg "add-right" $ \case
          (x, Vec xs) -> pure $ Vec (xs :|> x)
          _ -> throwErrorHere $ TypeError "add-left: second argument must be a vector"
      )

    -- Lists
    , ("cons", \case
          [x, List xs] -> pure $ List (x:xs)
          [_, _]       -> throwErrorHere $ TypeError "cons: second argument must be list"
          xs           -> throwErrorHere $ WrongNumberOfArgs "cons" 2 (length xs))
    , ("head", oneArg "head" $ \case
          List (x:_) -> pure x
          List []    -> throwErrorHere $ OtherError "head: empty list"
          _          -> throwErrorHere $ TypeError "head: expects list argument")
    , ("tail", oneArg "tail" $ \case
          List (_:xs) -> pure $ List xs
          List []     -> throwErrorHere $ OtherError "tail: empty list"
          _           -> throwErrorHere $ TypeError "tail: expects list argument")

    -- Lists and Vecs
    , ( "nth"
      , \case
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

    , ("lookup", \case
          [a, Dict m] -> pure $ case Map.lookup a m of
              Just v  -> v
              -- Probably an exception is better, but that seems cruel
              -- when you have no exception handling facilities.
              Nothing -> nil
          [_, d]      -> throwErrorHere $ TypeError $ "lookup: second argument must be a dict" <> renderCompactPretty d
          xs -> throwErrorHere $ WrongNumberOfArgs "lookup" 2 (length xs))
    , ( "safe-lookup"
      , twoArg "safe-lookup" $ \case
          (a, Dict m) -> pure $ case Map.lookup a m of
            Just v  -> toRad [kw "Just", v]
            Nothing -> kw "Nothing"
          (_, d) -> throwErrorHere $ TypeError $ "safe-lookup: second argument must be a dict" <> renderCompactPretty d
      )
    , ("string-append", \args ->
          let fromStr (String s) = Just s
              fromStr _          = Nothing
              ss = fromStr <$> args
          in if all isJust ss
              then pure . String . mconcat $ catMaybes ss
              else throwErrorHere $ TypeError "string-append: non-string argument")
    , ("insert", \case
          [k, v, Dict m] -> pure . Dict $ Map.insert k v m
          [_, _, _]                -> throwErrorHere

                                      $ TypeError "insert: third argument must be a dict"
          xs -> throwErrorHere $ WrongNumberOfArgs "insert" 3 (length xs))
    , ( "delete"
      , twoArg "delete" $ \case
          (k, Dict m) -> pure . Dict $ Map.delete k m
          _ -> throwErrorHere $ TypeError "delete: second argument must be a dict"
      )
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
    , ("<", \case
          [Number x, Number y] -> pure $ Boolean (x < y)
          [_, _]               -> throwErrorHere $ TypeError "<: expecting number"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "<" 2 (length xs))
    , (">", \case
          [Number x, Number y] -> pure $ Boolean (x > y)
          [_, _]               -> throwErrorHere $ TypeError ">: expecting number"
          xs                   -> throwErrorHere $ WrongNumberOfArgs ">" 2 (length xs))
    , ("foldl", \case
          [fn, init', v] -> do
            ls :: [Value] <- fromRadOtherErr v
            foldlM (\b a -> callFn fn [b, a]) init' ls
          -- [_, _, _]            -> throwErrorHere
          --                       $ TypeError "foldl: third argument should be a list"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "foldl" 3 (length xs))
    , ("foldr", \case
          [fn, init', v] -> do
            ls :: [Value] <- fromRadOtherErr v
            foldrM (\b a -> callFn fn [b, a]) init' ls
          -- [_, _, _]            -> throwErrorHere
          --                       $ TypeError "foldr: third argument should be a list"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "foldr" 3 (length xs))
    , ("map", \case
          [fn, List ls] -> List <$> traverse (callFn fn) (pure <$> ls)
          [_, _]        -> throwErrorHere $ TypeError "map: second argument should be a list"
          xs            -> throwErrorHere $ WrongNumberOfArgs "map" 3 (length xs))
    , ("keyword?", oneArg "keyword?" $ \case
          Keyword _ -> pure tt
          _         -> pure ff)
    , ("atom?", oneArg "atom?" $ \case
                  Atom _ -> pure tt
                  _      -> pure ff)
    , ("list?", oneArg "list?" $ \case
                  List _ -> pure tt
                  _      -> pure ff)
    , ("dict?", oneArg "dict?" $ \case
                  Dict _ -> pure tt
                  _      -> pure ff)
    , ( "type"
      , let kw' = pure . Keyword . Ident
        in oneArg "type" $ \case
             Atom _ -> kw' "atom"
             Keyword _ -> kw' "keyword"
             String _ -> kw' "string"
             Number _ -> kw' "number"
             Boolean _ -> kw' "boolean"
             List _ -> kw' "list"
             Vec _ -> kw' "vector"
             PrimFn _ -> kw' "function"
             Dict _ -> kw' "dict"
             Ref _ -> kw' "ref"
             Lambda{} -> kw' "function"
      )
    , ("string?", oneArg "string?" $ \case
          String _ -> pure tt
          _        -> pure ff)
    , ("boolean?", oneArg "boolean?" $ \case
          Boolean _ -> pure tt
          _         -> pure ff)
    , ("number?", oneArg "number?" $ \case
          Number _ -> pure tt
          _        -> pure ff)
    , ("member?", \case
          [x, List xs] -> pure . Boolean $ elem x xs
          [_, _]       -> throwErrorHere
                        $ TypeError "member?: second argument must be list"
          xs           -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("ref", oneArg "ref" newRef)
    , ("read-ref", oneArg "read-ref" $ \case
          Ref ref -> readRef ref
          _       -> throwErrorHere $ TypeError "read-ref: argument must be a ref")
    , ("write-ref", \case
          [Ref (Reference x), v] -> do
              st <- get
              put $ st { bindingsRefs = IntMap.insert x v $ bindingsRefs st }
              pure v
          [_, _]                 -> throwErrorHere
                                  $ TypeError "write-ref: first argument must be a ref"
          xs                     -> throwErrorHere
                                  $ WrongNumberOfArgs "write-ref" 2 (length xs))
    , ("show", oneArg "show" (pure . String . renderPrettyDef))
    , ( "seq"
      , oneArg "seq" $
          \case
            x@(List _) -> pure x
            x@(Vec _) -> pure x
            Dict kvs -> pure $ List [List [k, v] | (k,v) <- Map.toList kvs ]
            _ -> throwErrorHere $ TypeError "seq: can only be used on a list, vector or dictionary"
      )
    , ( "to-json"
      , oneArg "to-json" $ \v -> String . toS . Aeson.encode <$>
          maybeJson v ?? toLangError (OtherError "Could not serialise value to JSON")
      )
    , ( "default-ecc-curve",
        \case
          [] -> pure $ toRad defaultCurve
          xs -> throwErrorHere $ WrongNumberOfArgs "default-ecc-curve" 0 (length xs)
      )
    , ( "verify-signature"
      , \case
          [keyv, sigv, String msg] -> do
            key <- fromRadOtherErr keyv
            sig <- fromRadOtherErr sigv
            pure . Boolean $ verifySignature key sig msg
          [_, _, _] -> throwErrorHere $ OtherError "verify-signature: message must be a string"
          xs -> throwErrorHere $ WrongNumberOfArgs "verify-signature" 3 (length xs)
      )
    ]
  where

    fromRadOtherErr :: (FromRad Ann.WithPos a) => Value -> Lang m a
    fromRadOtherErr = hoistEither . first (toLangError . OtherError) . fromRad

    tt = Boolean True
    ff = Boolean False

    ok = kw "ok"

    kw = Keyword . Ident

    numBinop :: (Scientific -> Scientific -> Scientific)
             -> Text
             -> (Text, [Value] -> Lang m Value)
    numBinop fn name = (name, \case
        Number x:x':xs -> foldM go (Number x) (x':xs)
          where
            go (Number a) (Number b) = pure . Number $ fn a b
            go _ _ = throwErrorHere . TypeError
                   $ name <> ": expecting number"
        [Number _] -> throwErrorHere
                    $ OtherError $ name <> ": expects at least 2 arguments"
        _ -> throwErrorHere $ TypeError $ name <> ": expecting number")

-- * Helpers

-- Many primFns have a single argument.
oneArg :: Monad m => Text -> (Value -> Lang m Value) -> [Value] -> Lang m Value
oneArg fname f = \case
  [x] -> f x
  xs -> throwErrorHere $ WrongNumberOfArgs fname 1 (length xs)

twoArg :: Monad m => Text -> ((Value, Value) -> Lang m Value) -> [Value] -> Lang m Value
twoArg fname f = \case
  [x, y] -> f (x, y)
  xs -> throwErrorHere $ WrongNumberOfArgs fname 2 (length xs)

readValue
    :: (MonadError (LangError Value) m)
    => Text
    -> m Value
readValue s = do
    let p = parse "[read-primop]" s
    case p of
      Right v -> pure v
      Left e  -> throwErrorHere $ ThrownError (Ident "parse-error") (String e)

primFnsEnv :: PrimFns m -> Env Value
primFnsEnv pfs = Env (Map.fromList [ (pf, PrimFn pf) | pf <- Map.keys (getPrimFns pfs)])
