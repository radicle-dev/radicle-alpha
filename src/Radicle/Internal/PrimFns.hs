module Radicle.Internal.PrimFns where

import           Protolude hiding (TypeError)

import qualified Data.Aeson as Aeson
import           Data.Copointed (Copointed(..))
import qualified Data.IntMap as IntMap
import           Data.List (zip3)
import qualified Data.Map as Map
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.Exts (IsList(..), sortWith)
import           Text.Megaparsec (errorBundlePretty)

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Crypto
import qualified Radicle.Internal.Doc as Doc
import           Radicle.Internal.Eval
import           Radicle.Internal.Identifier (Ident(..), unsafeToIdent)
import qualified Radicle.Internal.Number as Num
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import qualified Radicle.Internal.Time as Time
import           Radicle.Internal.Type (Type(..))
import qualified Radicle.Internal.UUID as UUID

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: forall m. (Monad m) => Bindings (PrimFns m)
pureEnv =
    addPrimFns purePrimFns $ Bindings e mempty mempty 0 mempty 0 mempty 0
  where
    e = fromList . allDocs $
          [ ( "eval"
            , "The evaluation function used to evaluate inputs. Intially\
                \this is set to `base-eval`."
            , PrimFn $ unsafeToIdent "base-eval"
            )
          ]

-- | The added primitives override previously defined primitives and
-- variables with the same name.
addPrimFns  :: PrimFns m -> Bindings (PrimFns m) -> Bindings (PrimFns m)
addPrimFns primFns bindings =
    bindings { bindingsPrimFns = primFns <> bindingsPrimFns bindings
             , bindingsEnv = primFnsEnv <> bindingsEnv bindings
             }

  where
    primFnsEnv = Env $ Map.fromList
      $
      [ (pfn, Doc.Docd d (PrimFn pfn)) | (pfn, Doc.Docd d _) <- Map.toList (getPrimFns primFns)]


-- | The universal primops. These are available in chain evaluation.
purePrimFns :: forall m. (Monad m) => PrimFns m
purePrimFns = fromList $ allDocs $
    [ ( "base-eval"
      , "The default evaluation function. Expects an expression and a radicle\
        \ state. Return a list of length 2 consisting of the result of the\
        \ evaluation and the new state."
      , \case
          [expr, st] -> do
              originalBindings <- get
              setBindings st
              val <- baseEval expr
              st' <- get
              put originalBindings
              pure $ List [val, bindingsToRadicle st']
          xs -> throwErrorHere $ WrongNumberOfArgs "base-eval" 2 (length xs)
      )
    , ( "pure-state"
      , "Returns a pure initial radicle state. This is the state of a radicle\
        \ chain before it has processed any inputs."
      , \case
          [] -> pure $ bindingsToRadicle (pureEnv :: Bindings (PrimFns m))
          xs -> throwErrorHere $ WrongNumberOfArgs "pure-state" 0 (length xs)
      )
    , ( "state->env"
      , "Extract the environment from a radicle state."
      , oneArg "state->env" $ \case
          VState s -> pure (VEnv (stateEnv s))
          v -> throwErrorHere $ TypeError "state->env" 0 TState v
      )
    , ( "set-binding"
      , "Add a binding to a radicle env."
      , threeArg "set-binding" $ \case
          (Atom i, v, VEnv (Env m)) -> pure $ VEnv (Env (Map.insert i (Doc.Docd Nothing v) m))
          (Atom _, _, e) -> throwErrorHere $ TypeError "set-binding" 2 TEnv e
          (a, _, _) -> throwErrorHere $ TypeError "set-binding" 0 TAtom a
      )
    , ( "get-binding"
      , "Lookup a binding in a radicle env."
      , twoArg "get-binding" $ \case
          (Atom i@(Ident t), VEnv (Env m)) -> case Map.lookup i m of
            Just v -> pure (copoint v)
            Nothing -> throwErrorHere $ OtherError $ "get-binding: " <> t <> " was not in the input env."
          (Atom _, v) -> throwErrorHere $ TypeError "get-binding" 1 TEnv v
          (v, _) -> throwErrorHere $ TypeError "get-binding" 0 TAtom v
      )
    , ( "set-env"
      , "Sets the environment of a radicle state to a new value. Returns the updated state."
      , twoArg "set-env" $ \case
          (VEnv e, VState s) -> pure $ VState $ s { stateEnv = e }
          (VEnv _, v) -> throwErrorHere $ TypeError "set-env" 1 TState v
          (v, _) -> throwErrorHere $ TypeError "set-env" 0 TEnv v
      )
    , ( "apply"
      , "Calls the first argument (a function) using as arguments the\
        \ elements of the the second argument (a list)."
      , \case
          [fn, List args] -> callFn fn args
          [_, v]          -> throwErrorHere $ TypeError "apply" 1 TList v
          xs -> throwErrorHere $ WrongNumberOfArgs "apply" 2 (length xs))
    , ( "read-annotated"
      , "`(read-anotated label s)` parses the string `s` into a radicle value.\
        \ The resulting value is not evaluated. The `label` argument is a string\
        \ which is used to annotate the value with line numbers."
      , twoArg "read-annotated" $ \case
          (String label, String s) -> readValue label s
          (String _, v) -> throwErrorHere $ TypeError "read" 1 TString v
          (v, _) -> throwErrorHere $ TypeError "read" 0 TString v
      )
    , ( "read-many-annotated"
      , "(read-many-annotated label s) parses a string into a vector of radicle\
        \ values. The resulting values are not evaluated. The `label` argument \
        \is a string which is used to annotate the values with line numbers."
      , twoArg "read-many" $ \case
          (String label, String s) -> Vec . Seq.fromList <$> readValues label s
          (String _, v) -> throwErrorHere $ TypeError "read-many" 1 TString v
          (v, _) -> throwErrorHere $ TypeError "read-many" 0 TString v
      )
    , ("get-current-state"
      , "Returns the current radicle state."
      , \case
          [] -> gets bindingsToRadicle
          xs -> throwErrorHere $ WrongNumberOfArgs "get-current-state" 0 (length xs))
    , ( "set-current-state"
      , "Replaces the radicle state with the one provided."
      , oneArg "set-current-state" $ \x -> do
          setBindings x
          pure ok
      )
    , ("list"
      , "Turns the arguments into a list."
      , pure . List)
    , ("dict"
      , "Given an even number of arguments, creates a dict where the `2i`-th argument\
        \ is the key for the `2i+1`th argument. If one of the even indexed arguments\
        \ is not hashable then an exception is thrown."
      , evenArgs "dict" >=> dict . Map.fromList
      )
    , ("throw"
      , "Throws an exception. The first argument should be an atom used as a label for\
        \ the exception, the second can be any value."
      , \case
          [Atom label, exc] -> throwErrorHere $ ThrownError label exc
          [v, _]            -> throwErrorHere $ TypeError "throw" 0 TAtom v
          xs                -> throwErrorHere $ WrongNumberOfArgs "throw" 2 (length xs))
    , ( "eq?"
      , "Checks if two values are equal."
      , \case
          [a, b] -> pure $ Boolean (a == b)
          xs     -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))

    -- Vectors
    , ( "add-right"
      , "Adds an element to the right side of a vector."
      , twoArg "add-right" $ \case
          (x, Vec xs) -> pure $ Vec (xs :|> x)
          (_, v) -> throwErrorHere $ TypeError "add-right" 1 TVec v
      )

    -- Sequences: Lists, Vecs and Strings
    , ("cons"
      , "Adds an element to the front of a sequence."
      , twoArg "cons" $ \case
          (x, List xs) -> pure $ List (x:xs)
          (x, Vec xs) -> pure $ Vec (x Seq.:<| xs)
          (isCharV -> Just c, String s) -> pure $ String (T.cons c s)
          (_, String _) -> throwErrorHere $ OtherError "cons: when consing onto a string the first argument must be a single-character string"
          (_, _)       -> throwErrorHere $ OtherError "cons: second argument must be a list, vector or string"
      )
    , ("first"
      , "Retrieves the first element of a sequence if it exists. Otherwise throws an\
        \ exception."
      , oneArg "first" $ \case
          List (x:_)                      -> pure x
          List _                          -> throwErrorHere $ OtherError "first: empty list"
          Vec (x Seq.:<| _)               -> pure x
          Vec _                           -> throwErrorHere $ OtherError "first: empty vector"
          String (T.uncons -> Just (c,_)) -> pure (String (T.singleton c))
          String _                        -> throwErrorHere $ OtherError "first: empty string"
          v                               -> throwErrorHere $ TypeError "first" 0 TSequence v)
    , ("rest"
      , "Given a non-empty sequence, returns the sequence of all the elements but the\
        \ first. If the sequence is empty, throws an exception."
      , oneArg "tail" $ \case
          List (_:xs)                     -> pure $ List xs
          List _                          -> throwErrorHere $ OtherError "rest: empty list"
          Vec (_ Seq.:<| xs)              -> pure $ Vec xs
          Vec _                           -> throwErrorHere $ OtherError "rest: empty vector"
          String (T.uncons -> Just (_,t)) -> pure $ String t
          String _                        -> throwErrorHere $ OtherError "rest: empty string"
          v                               -> throwErrorHere $ TypeError "rest" 0 TSequence v)
    , ( "length"
      , "Returns the length of a sequence.."
      , oneArg "length" $ \case
          List xs -> pure . Number . fromIntegral . length $ xs
          Vec xs -> pure . Number . fromIntegral . length $ xs
          String s -> pure . Number . fromIntegral . T.length $ s
          v -> throwErrorHere $ TypeError "length" 0 TSequence v
      )
    , ( "drop"
      , "Returns all but the first `n` items of a sequence, unless the sequence is empty,\
        \ in which case an exception is thrown."
      , twoArg "drop" $ \case
          (Number n, vs) -> case Num.isInt n of
            Left _ -> throwErrorHere $ OtherError "drop: first argument must be an int"
            Right i -> case vs of
              List xs   -> pure . List $ drop i xs
              Vec xs    -> pure . Vec $ Seq.drop i xs
              String xs -> pure . String $ T.drop i xs
              v         -> throwErrorHere $ TypeError "drop" 1 TSequence v
          (v, _) -> throwErrorHere $ TypeError "drop" 0 TNumber v
      )
    , ( "take"
      , "Returns the first `n` items of a sequence, unless the sequence is too short,\
        \ in which case an exception is thrown."
      , twoArg "take" $ \case
          (Number n, vs) -> case Num.isInt n of
            Left _ -> throwErrorHere $ OtherError "take: first argument must be an integer"
            Right i -> case vs of
              List xs   -> pure . List $ take i xs
              Vec xs    -> pure . Vec $ Seq.take i xs
              String xs -> pure . String $ T.take i xs
              _         -> throwErrorHere $ TypeError "take" 1 TSequence vs
          (v, _) -> throwErrorHere $ TypeError "take" 0 TNumber v
      )
    , ( "nth"
      , "Given an integral number `n` and a sequence `xs`, returns the `n`th\
        \ element (zero indexed) of `xs`. If `xs` does not have an `n`-th\
        \ element, or if it is not a sequence, then an exception is thrown."
      , \case
          [Number n, v] -> case Num.isInt n of
            Left _ -> throwErrorHere $ OtherError "nth: first argument was not an integer"
            Right i -> do
              x_ <- case v of
                Vec xs   -> pure $ Seq.lookup i xs
                List xs  -> pure $ xs `atMay` i
                String t -> pure $ atMay (charV <$> toS t) i
                _        -> throwErrorHere $ TypeError "nth" 1 TSequence v
              case x_ of
                Just x -> pure x
                Nothing -> throwErrorHere $ OtherError "nth: index out of bounds"
          [v,_] -> throwErrorHere $ TypeError "nth" 0 TNumber v
          xs -> throwErrorHere $ WrongNumberOfArgs "nth" 2 (length xs)
      )
    , ( "sort-by"
      , "Given a sequence `xs` and a function `f`, returns a sequence with the same\
        \ elements `x` of `xs` but sorted according to `(f x)`."
      , twoArg "sort-by" $ \case
          (f, List xs)  -> List <$> sortSeqWith identity sortWith f xs
          (f, Vec xs)   -> Vec <$> sortSeqWith identity Seq.sortOn f xs
          (f, String t) -> String . toS <$> sortSeqWith charV sortWith f (toS t)
          (_, v)        -> throwErrorHere $ TypeError "sort-by" 1 TSequence v
      )
    , ( "zip"
      , "Takes two sequences and returns a sequence of corresponding pairs. In one\
        \ sequence is shorter than the other, the excess elements of the longer\
        \ sequence are discarded. If both sequences have the same type, then the\
        \ result will have this type also. If the sequences don't have the same\
        \ type, the result is a vector."
      , twoArg "zip" $ \case
          (Vec xs, Vec ys) -> pure $ Vec (pair <$> Seq.zip xs ys)
          (List xs, List ys) -> pure $ List (pair <$> zip xs ys)
          (String x, String y) -> pure $ Vec $ fromList $ (pair . bimap charV charV <$> T.zip x y)
          (x, y) -> case (isSeq x, isSeq y) of
            (Just xs, Just ys) -> pure $ Vec $ fromList $ (pair <$> zip xs ys)
            (Just _, _) -> throwErrorHere $ TypeError "zip" 1 TSequence y
            _ -> throwErrorHere $ TypeError "zip" 0 TSequence x
      )
    , ( "vec-to-list"
      , "Transforms vectors to lists."
      , oneArg "vec-to-list" $ \case
          Vec xs -> pure (List (Protolude.toList xs))
          v -> throwErrorHere $ TypeError "vec-to-list" 0 TVec v
      )
    , ( "list-to-vec"
      , "Transforms lists into vectors."
      , oneArg "list-to-vec" $ \case
          List xs -> pure (Vec (Seq.fromList xs))
          v -> throwErrorHere $ TypeError "list-to-vec" 0 TList v
      )

    -- Dicts
    , ("lookup"
      , "Given a value `k` (the 'key') and a dict `d`, returns the value associated\
        \ with `k` in `d`. If the key does not exist in `d` then `()` is returned\
        \ instead. If `d` is not a dict then an exception is thrown."
      , \case
          [a, Dict m] -> case Map.lookup a m of
              Just v  -> pure v
              Nothing -> throwErrorHere $ OtherError $ "lookup: key did not exist: " <> renderCompactPretty a
          [_, d]      -> throwErrorHere $ TypeError "lookup" 1 TDict d
          xs -> throwErrorHere $ WrongNumberOfArgs "lookup" 2 (length xs))
    , ( "map-values"
      , "Given a function `f` and a dict `d`, returns a dict with the same keys as `d`\
        \ but `f` applied to all the associated values."
      , twoArg "map-values" $ \case
          (f, Dict m) -> do
            let kvs = Map.toList m
            vs <- traverse (\v -> callFn f [v]) (snd <$> kvs)
            pure . Dict . Map.fromList $ zip (fst <$> kvs) vs
          (_, v) -> throwErrorHere $ TypeError "map-values" 1 TDict v
      )
    , ( "map-keys"
      , "Given a function `f` and a dict `d`, returns a dict with the same values as `d`\
        \ but `f` applied to all the keys.\
        \ If `f` maps two keys to the same thing, the greatest key and value are kept."
      , twoArg "map-keys" $ \case
          (f, Dict m) -> do
            let kvs = Map.toList m
            vs <- traverse (\v -> callFn f [v]) (fst <$> kvs)
            dict . Map.fromList $ zip vs (snd <$> kvs)
          (_, v) -> throwErrorHere $ TypeError "map-keys" 1 TDict v
      )

    -- Structures
    , ( "<>"
      , "Merges two structures together. On sequences this performs\
        \ concatenation. On dicts this performs the right-biased merge. If the\
        \ inputs are sequences of the same type, then the result will be of this\
        \ same type. If the inputs are sequences of different types, then the\
        \ result will be a vector."
      , twoArg "<>" $ \case
          (List xs, List ys) -> pure $ List (xs ++ ys)
          (Vec xs, Vec ys) -> pure $ Vec (xs Seq.>< ys)
          (Dict m, Dict n) -> pure $ Dict (n <> m)
          (Dict _, v) -> throwErrorHere $ TypeError "<>" 1 TDict v
          (String x, String y) -> pure $ String (T.append x y)
          (x, y) -> case (isSeq x, isSeq y) of
            (Just xs, Just ys) -> pure $ Vec $ fromList $ xs ++ ys
            (Just _, _) -> throwErrorHere $ TypeError "<>" 1 TSequence y
            _ -> throwErrorHere $ TypeError "<>" 0 TSequence x
      )

    , ( "string-length"
      , "DEPRECATED Use `length` instead. Returns the length of a string."
      , oneArg "string-length" $ \case
          String s -> pure . Number . fromIntegral . T.length $ s
          v -> throwErrorHere $ TypeError "string-length" 0 TString v
      )
    , ( "string-append"
      , "Concatenates a variable number of string arguments. If one of the arguments\
        \ isn't a string then an exception is thrown."
      , \args ->
          let fromStr (String s) = Just s
              fromStr _          = Nothing
              ss = zip3 args (fromStr <$> args) [0..]
          in case find (\(_, s_, _) -> isNothing s_) ss of
               Just (v, _, i) -> throwErrorHere $ TypeError "string-append" i TString v
               Nothing -> pure . String . mconcat $ catMaybes $ (\(_,x,_) -> x) <$> ss
      )
    , ( "string-replace"
      , "Replace all occurrences of the first argument with the second in the third."
      , threeArg "string-replace" $ \case
         (String old, String new, String str) -> pure $ String $ T.replace old new str
         (String _, String _, v) -> throwErrorHere $ TypeError "string-replace" 2 TString v
         (String _, v, _) -> throwErrorHere $ TypeError "string-replace" 1 TString v
         (v, _, _) -> throwErrorHere $ TypeError "string-replace" 0 TString v
      )
    , ( "insert"
      , "Given `k`, `v` and a dict `d`, returns a dict with the same associations\
        \ as `d` but with `k` associated to `d`. If `d` isn't a dict or if `k` isn't\
        \ hashable then an exception is thrown."
      , threeArg "insert" $ \case
          (k, v, Dict m) ->
            if hashable k
            then pure . Dict $ Map.insert k v m
            else throwErrorHere NonHashableKey
          (_, _, v)      -> throwErrorHere $ TypeError "insert" 2 TDict v
      )
    , ( "delete"
      , "Given `k` and a dict `d`, returns a dict with the same associations as `d` but\
        \ without the key `k`. If `d` isn't a dict then an exception is thrown."
      , twoArg "delete" $ \case
          (k, Dict m) -> pure . Dict $ Map.delete k m
          (_, v) -> throwErrorHere $ TypeError "delete" 1 TDict v
      )
    -- The semantics of + and - in Scheme is a little messed up. (+ 3)
    -- evaluates to 3, and of (- 3) to -3. That's pretty intuitive.
    -- But while (+ 3 2 1) evaluates to 6, (- 3 2 1) evaluates to 0. So with -
    -- it is *not* correct to say that it's a foldl (-) 0. Instead, it
    -- special-cases on one-argument application. (Similarly with * and /.)
    --
    -- In order to avoid this sort of thing, we don't allow +,*,- and / to be
    -- applied to a single argument.
    , numBinop (+) "+" "Adds two numbers together."
    , numBinop (*) "*" "Multiplies two numbers together."
    , numBinop (-) "-" "Substracts one number from another."
    , ( "/"
      , "Divides one number by another. Throws an exception if the second argument is 0."
      , twoArg "/" $ \case
          (Number x, Number y) | y /= 0 -> pure $ Number (x / y)
          (Number _, Number _) -> throwErrorHere $ OtherError "Can't divide by 0"
          (Number _, v) -> throwErrorHere $ TypeError "/" 1 TNumber v
          (v, _) -> throwErrorHere $ TypeError "/" 0 TNumber v
      )
    , ( "<"
      , "Checks if a number is strictly less than another."
      , \case
          [Number x, Number y] -> pure $ Boolean (x < y)
          [Number _, v]        -> throwErrorHere $ TypeError "<" 1 TNumber v
          [v, _]               -> throwErrorHere $ TypeError "<" 0 TNumber v
          xs                   -> throwErrorHere $ WrongNumberOfArgs "<" 2 (length xs))
    , ( ">"
      , "Checks if a number is strictly greater than another."
      , \case
          [Number x, Number y] -> pure $ Boolean (x > y)
          [Number _, v]        -> throwErrorHere $ TypeError ">" 1 TNumber v
          [_, v]               -> throwErrorHere $ TypeError ">" 0 TNumber v
          xs                   -> throwErrorHere $ WrongNumberOfArgs ">" 2 (length xs))
    , ( "integral?"
      , "Checks if a number is an integer."
      , oneArg "integral?" $ \case
          Number n -> case Num.isInteger n of
            Left _  -> pure ff
            Right _ -> pure tt
          v -> throwErrorHere $ TypeError "integral?" 0 TNumber v
      )
    , ( "foldl"
      , "Given a function `f`, an initial value `i` and a sequence (list, vector\
        \ or string) `xs`, reduces `xs` to a single value by starting with `i`\
        \ and repetitively combining values with `f`, using elements of `xs`\
        \ from left to right."
      , threeArg "foldl" $ \case
          (fn, ini, s) -> case s of
            Vec xs -> foldlM (\x y -> callFn fn [x, y]) ini xs
            List xs -> foldlM (\x y -> callFn fn [x, y]) ini xs
            String t -> T.foldl (\macc c -> do {x <- macc; callFn fn [x, String (T.singleton c)]}) (pure ini) t
            v -> throwErrorHere $ TypeError "foldl" 2 TSequence v
      )
    , ( "foldr"
      , "Given a function `f`, an initial value `i` and a sequence (list, vector\
        \ or string) `xs`, reduces `xs` to a single value by starting with `i`\
        \ and repetitively combining values with `f`, using elements of `xs`\
        \ from right to left."
      , threeArg "foldr" $ \case
          (fn, ini, s) -> case s of
            Vec xs -> foldrM (\x y -> callFn fn [x, y]) ini xs
            List xs -> foldrM (\x y -> callFn fn [x, y]) ini xs
            String t -> T.foldr (\c macc -> do {x <- macc; callFn fn [String (T.singleton c), x]}) (pure ini) t
            v -> throwErrorHere $ TypeError "foldr" 2 TSequence v
      )
    , ( "keyword?"
      , isTy "keyword"
      , oneArg "keyword?" $ \case
          Keyword _ -> pure tt
          _         -> pure ff
      )
    , ( "atom?"
      , isTy "atom"
      , oneArg "atom?" $ \case
          Atom _ -> pure tt
          _      -> pure ff
      )
    , ( "list?"
      , isTy "list"
      , oneArg "list?" $ \case
          List _ -> pure tt
          _      -> pure ff
      )
    , ( "vector?"
      , isTy "vector"
      , oneArg "vector?" $ \case
          Vec _ -> pure tt
          _     -> pure ff
      )
    , ( "dict?"
      , isTy "dict"
      , oneArg "dict?" $ \case
          Dict _ -> pure tt
          _      -> pure ff
      )
    , ( "type"
      , "Returns a keyword representing the type of the argument; one of:\
        \ `:atom`, `:keyword`, `:string`, `:number`, `:boolean`, `:list`,\
        \ `:vector`, `:function`, `:dict`, `:ref`."
      , oneArg "type" $ pure . typeToValue . valType
      )
    , ( "string?"
      , isTy "string"
      , oneArg "string?" $ \case
          String _ -> pure tt
          _        -> pure ff
      )
    , ( "boolean?"
      , isTy "boolean"
      , oneArg "boolean?" $ \case
          Boolean _ -> pure tt
          _         -> pure ff
      )
    , ( "number?"
      , isTy "number"
      , oneArg "number?" $ \case
          Number _ -> pure tt
          _        -> pure ff
      )
    , ( "member?"
      , "Given `k` and dict `d`, checks if `k` is a key in `d`."
      , twoArg "member?" $ \case
          (x, Dict m)  -> pure . Boolean $ Map.member x m
          (_, v)       -> throwErrorHere
                        $ TypeError "member?" 1 TDict v
      )
    , ( "elem?"
      , "Given `v` and a sequence `s`, checks if `v` is one of the items of `s`."
      , twoArg "member?" $ \case
          (x, List xs) -> pure . Boolean $ elem x xs
          (x, Vec xs)  -> pure . Boolean . isJust $ Seq.elemIndexL x xs
          (isCharV -> Just c, String t) -> pure . Boolean $ T.any (== c) t
          (_, v)       -> throwErrorHere
                        $ TypeError "elem?" 1 TSequence v
      )
    , ( "ref"
      , "Creates a ref with the argument as the initial value."
      , oneArg "ref" newRef)
    , ( "read-ref"
      , "Returns the current value of a ref."
      , oneArg "read-ref" $ \case
          Ref ref -> readRef ref
          v       -> throwErrorHere $ TypeError "read-ref" 0 TRef v)
    , ( "write-ref"
      , "Given a reference `r` and a value `v`, updates the value stored in `r` to be\
        \ `v` and returns `v`."
      , \case
          [Ref (Reference x), v] -> do
              st <- get
              put $ st { bindingsRefs = IntMap.insert x v $ bindingsRefs st }
              pure v
          [v, _]                 -> throwErrorHere
                                  $ TypeError "write-ref" 0 TRef v
          xs                     -> throwErrorHere
                                  $ WrongNumberOfArgs "write-ref" 2 (length xs))
    , ( "show"
      , "Returns a string representing the argument value."
      , oneArg "show" (pure . String . renderPrettyDef))
    , ( "seq"
      , "Given a structure `s`, returns a sequence. Lists, vectors and strings\
        \ are returned without modification while for dicts a vector of\
        \ key-value-pairs is returned: these are vectors of length 2 whose first\
        \ item is a key and whose second item is the associated value."
      , oneArg "seq" $
          \case
            x@(List _) -> pure x
            x@(Vec _) -> pure x
            x@(String _) -> pure x
            Dict kvs -> pure . Vec . Seq.fromList $ [Vec (Seq.fromList [k, v]) | (k,v) <- Map.toList kvs ]
            v -> throwErrorHere $ TypeError "seq" 0 TStructure v
      )
    , ( "to-json"
      , "Returns a JSON formatted string representing the input value."
      , oneArg "to-json" $ \v -> String . toS . Aeson.encode <$>
          maybeJson v ?? toLangError (OtherError "Could not serialise value to JSON")
      )
    , ( "default-ecc-curve"
      , "Returns the default elliptic-curve used for generating cryptographic keys."
      ,
        \case
          [] -> pure $ toRad defaultCurve
          xs -> throwErrorHere $ WrongNumberOfArgs "default-ecc-curve" 0 (length xs)
      )
    , ( "verify-signature"
      , "Given a public key `pk`, a signature `s` and a message (string) `m`, checks\
        \ that `s` is a signature of `m` for the public key `pk`."
      , threeArg "verify-signature" $ \case
          (keyv, sigv, String msg) -> do
            key <- fromRadOtherErr keyv
            sig <- fromRadOtherErr sigv
            pure . Boolean $ verifySignature key sig msg
          (_, _, _) -> throwErrorHere
                     $ OtherError "verify-signature: message must be a string"
      )
    , ( "public-key?"
      , "Checks if a value represents a valid public key."
      , oneArg "public-key?" $
          \v -> case fromRad v of
                  Right (_ :: PublicKey) -> pure tt
                  Left _                 -> pure ff
      )
    , ( "uuid?"
      , "Checks if a string has the format of a UUID."
      , oneArg "uuid?" $ \case
          String t -> pure . Boolean . UUID.isUUID $ t
          v -> throwErrorHere $ TypeError "uuid?" 0 TString v
      )
    , ( "doc"
      , "Returns the documentation string for a variable. To print it instead, use `doc!`."
      , oneArg "doc" $ \case
          Atom i -> do d <- lookupAtomDoc i
                       pure . String $
                         fromMaybe ("No documentation found for " <> fromIdent i <> ".") d
          _ -> throwErrorHere $ OtherError "doc: expects an atom"
      )
    , ( "match-pat"
      , "The most basic built-in pattern-matching dispatch function."
      , twoArg "match-pat" $ \case
          (x@(Atom _), v) -> pure $ toRad (Just (Dict (Map.singleton x v)))
          (pat, v) -> callFn pat [v]
      )
    , ( "import"
      , "Import a module, making all the definitions of that module available\
        \ in the current scope. The first argument must be a module to import.\
        \ Two optional arguments affect how and which symbols are imported.\
        \ `(import m :as 'foo)` will import all the symbols of `m` with the prefix\
        \ `foo/`. `(import m '[f g])` will only import `f` and `g` from `m`.\
        \ `(import m '[f g] :as 'foo')` will import `f` and `g` from `m` as `foo/f`\
        \ and `foo/g`. To import definitions with no qualification at all, use\
        \ `(import m :unqualified)`."
      , \case
          [v]                                           -> import' v Nothing FullyQualified
          [v, Vec these]                                -> import' v (Just these) FullyQualified
          [v, Keyword (Ident "as"), Atom q]             -> import' v Nothing (Qualified q)
          [v, Vec these, Keyword (Ident "as"), Atom q]  -> import' v (Just these) (Qualified q)
          [v, Keyword (Ident "unqualified")]            -> import' v Nothing Unqualified
          [v, Vec these, Keyword (Ident "unqualified")] -> import' v (Just these) Unqualified
          _ -> throwErrorHere $ OtherError "import: expects a module, an optional list of symbols to import, and an optional qualifier."
      )
    , ( "timestamp?"
      , "Returns true if the input is an ISO 8601 formatted Coordinated\
        \Universal Time (UTC) timestamp string. If the input isn't a string, an\
        \ exception is thrown."
      , oneArg "timestamp?" $ \case
          String s -> case Time.parseTime s of
            Just _  -> pure tt
            Nothing -> pure ff
          v -> throwErrorHere $ TypeError "timestamp?" 0 TString v
      )
    , ( "unix-epoch"
      , "Given an ISO 8601 formatted Coordinated Universal Time (UTC) timestamp\
        \, returns the corresponding Unix epoch time, i.e., the number of\
        \ seconds since Jan 01 1970 (UTC)."
      , oneArg "unix-epoch" $ \case
          String s -> do
            utc <- Time.parseTime s ?? toLangError (OtherError "Invalid UTC timestamp.")
            ss <- Time.unixSeconds utc ?? toLangError (OtherError "Could not convert UTC time to Unix epoch.")
            pure (Number (fromIntegral ss))
          v -> throwErrorHere $ TypeError "unix-epoch" 0 TString v
      )
    ]
  where
    isTy t = "Checks if the argument is a " <> t <> "."

    fromRadOtherErr :: (FromRad Ann.WithPos a) => Value -> Lang m a
    fromRadOtherErr = hoistEither . first (toLangError . OtherError) . fromRad

    tt = Boolean True
    ff = Boolean False

    ok = kw "ok"

    kw = Keyword . Ident

    charV = String . T.singleton

    -- | O(1) Checks if a 'Text' is a single 'Char'.
    isChar t = do
      (c, rest) <- T.uncons t
      guard $ T.null rest
      pure c

    isCharV (String (isChar -> Just c)) = pure c
    isCharV _                           = Nothing

    isSeq (Vec s)    = pure (Protolude.toList s)
    isSeq (List xs)  = pure xs
    isSeq (String t) = pure (charV <$> toS t)
    isSeq _          = Nothing

    sortSeqWith el sorter f xs = do
      ys <- traverse (\x -> (x,) <$> callFn f [el x]) xs
      let ys' = sorter snd ys
      pure (fst <$> ys')

    pair :: (Value, Value) -> Value
    pair (x,y) = Vec (x :<| y :<| Empty)

    numBinop :: (Rational -> Rational -> Rational)
             -> Text
             -> Text
             -> (Text, Text, [Value] -> Lang m Value)
    numBinop fn name doc =
      ( name
      , doc
      , twoArg name $ \case
          (Number a, Number b) -> pure . Number $ fn a b
          (Number _, v) -> throwErrorHere $ TypeError name 1 TNumber v
          (v, _) -> throwErrorHere $ TypeError name 0 TNumber v
      )

    import' (Dict d) v_ qual = do
      v <- kwLookup "env" d ?? toLangError (OtherError "Modules should have an `:env` key")
      n <- kwLookup "module" d ?? toLangError (OtherError "Modules should have an `:module` key")
      case (n,v) of
        (Atom name, VEnv e) -> do
          let allMod = fromEnv e
          toImport <- case v_ of
                Just vs -> do
                  is <- Protolude.toList <$> traverse isAtom vs ?? toLangError (OtherError "import: must be given a vector of symbols to import")
                  pure $ Map.restrictKeys allMod (Set.fromList is)
                Nothing -> pure allMod
          let qualifier = case qual of
                Qualified q    -> ((q <> Ident "/") <>)
                Unqualified    -> identity
                FullyQualified -> ((name <> Ident "/") <>)
          let qualified = Env $ Map.mapKeysMonotonic qualifier toImport
          s <- get
          put $ s { bindingsEnv = qualified <> bindingsEnv s }
          pure ok
        _ -> throwErrorHere (OtherError "The `:module` key of a module should be an `:atom`, and the `:env` key should be an `:env`.")
    import' _ _ _ = throwErrorHere (OtherError "Modules must be dicts")

data ImportQual = Unqualified | FullyQualified | Qualified Ident

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

threeArg
    :: Monad m
    => Text
    -> ((Value, Value, Value) -> Lang m Value)
    -> [Value]
    -> Lang m Value
threeArg fname f = \case
  [x, y, z] -> f (x, y, z)
  xs -> throwErrorHere $ WrongNumberOfArgs fname 3 (length xs)

readValue
    :: (MonadError (LangError Value) m)
    => Text
    -> Text
    -> m Value
readValue sourceFile code = do
    case parse sourceFile code of
      Right v -> pure v
      Left e  -> throwErrorHere $ ThrownError (Ident "parse-error") (String e)

readValues
    :: (MonadError (LangError Value) m)
    => Text
    -> Text
    -> m [Value]
readValues sourceFile code = do
    case parseValues sourceFile code  of
      Right vs -> pure vs
      Left e  -> throwErrorHere $ ThrownError (Ident "parse-error") (String . toS $ errorBundlePretty e)

allDocs :: [(Text, Text, a)] -> [(Ident, Maybe Text, a)]
allDocs = fmap $ \(x,y,z) -> (unsafeToIdent x, Just y, z)
