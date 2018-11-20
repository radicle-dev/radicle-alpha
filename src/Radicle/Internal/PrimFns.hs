module Radicle.Internal.PrimFns where

import           Protolude hiding (TypeError)

import qualified Data.Aeson as Aeson
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           GHC.Exts (IsList(..), sortWith)
import           Text.Megaparsec (parseErrorPretty)

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Crypto
import qualified Radicle.Internal.Doc as Doc
import qualified Radicle.Internal.Number as Num
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import qualified Radicle.Internal.UUID as UUID

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: forall m. (Monad m) => Bindings (PrimFns m)
pureEnv =
    addPrimFns purePrimFns $ Bindings e mempty mempty 0
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
      , "Returns a pure initial radicle state. This is the state of a radicle\
        \ chain before it has processed any inputs."
      , \case
          [] -> pure $ toRad (pureEnv :: Bindings (PrimFns m))
          xs -> throwErrorHere $ WrongNumberOfArgs "pure-env" 0 (length xs)
      )
    , ("apply"
      , "Calls the first argument (a function) using as arguments the\
        \ elements of the the second argument (a list)."
      , \case
          [fn, List args] -> callFn fn args
          [_, _]          -> throwErrorHere $ TypeError "apply: expecting list as second arg"
          xs -> throwErrorHere $ WrongNumberOfArgs "apply" 2 (length xs))
    , ( "read"
      , "Parses a string into a radicle value. Does not evaluate the value."
      , oneArg "read" $ \case
          String s -> readValue s
          _ -> throwErrorHere $ TypeError "read: expects string"
      )
    , ( "read-many"
      , "Parses a string into a vector of radicle values. Does not evaluate the values."
      , oneArg "read-many" $ \case
          String s -> readValues s
          _ -> throwErrorHere $ TypeError "read-many: expects string"
      )
    , ("get-current-env"
      , "Returns the current radicle state."
      , \case
          [] -> toRad <$> get
          xs -> throwErrorHere $ WrongNumberOfArgs "get-current-env" 0 (length xs))
    , ( "set-current-env"
      , "Replaces the radicle state with the one provided."
      , oneArg "set-current-env" $ \x -> do
          e' :: Bindings () <- fromRadOtherErr x
          e <- get
          put e { bindingsEnv = bindingsEnv e'
                , bindingsNextRef = bindingsNextRef e'
                , bindingsRefs = bindingsRefs e'
                }
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
          [_, _]            -> throwErrorHere $ TypeError "throw: first argument must be atom"
          xs                -> throwErrorHere $ WrongNumberOfArgs "throw" 2 (length xs))
    , ( "eq?"
      , "Checks if two values are equal."
      , \case
          [a, b] -> pure $ Boolean (a == b)
          xs     -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))

    -- Vectors
    , ( "add-left"
      , "Adds an element to the left side of a vector."
      , twoArg "add-left" $ \case
          (x, Vec xs) -> pure $ Vec (x :<| xs)
          _ -> throwErrorHere $ TypeError "add-left: second argument must be a vector"
      )
    , ( "add-right"
      , "Adds an element to the right side of a vector."
      , twoArg "add-right" $ \case
          (x, Vec xs) -> pure $ Vec (xs :|> x)
          _ -> throwErrorHere $ TypeError "add-right: second argument must be a vector"
      )

    -- Lists
    , ("cons"
      , "Adds an element to the front of a list."
      , \case
          [x, List xs] -> pure $ List (x:xs)
          [_, _]       -> throwErrorHere $ TypeError "cons: second argument must be list"
          xs           -> throwErrorHere $ WrongNumberOfArgs "cons" 2 (length xs))
    , ("head"
      , "Retrieves the first element of a sequence if it exists. Otherwise throws an\
        \ exception."
      , oneArg "head" $ \case
          List (x:_)        -> pure x
          List []           -> throwErrorHere $ OtherError "head: empty list"
          Vec (x Seq.:<| _) -> pure x
          Vec Seq.Empty     -> throwErrorHere $ OtherError "head: empty vector"
          _                 -> throwErrorHere $ TypeError "head: expects sequence argument")
    , ("tail"
      , "Given a non-empty sequence, returns the sequence of all the elements but the\
        \ first. If the sequence is empty, throws an exception."
      , oneArg "tail" $ \case
          List (_:xs)        -> pure $ List xs
          List []            -> throwErrorHere $ OtherError "tail: empty list"
          Vec (_ Seq.:<| xs) -> pure $ Vec xs
          Vec Seq.Empty      -> throwErrorHere $ OtherError "tail: empty vector"
          _                  -> throwErrorHere $ TypeError "tail: expects sequence argument")

    -- Lists and Vecs
    , ( "drop"
      , "Returns all but the first `n` items of a sequence, unless the sequence is empty,\
        \ in which case an exception is thrown."
      , twoArg "drop" $ \case
          (Number n, vs) -> case Num.isInt n of
            Left _ -> throwErrorHere $ OtherError "drop: first argument must be an int"
            Right i -> case vs of
              List xs -> pure . List $ drop i xs
              Vec xs -> pure . Vec $ Seq.drop i xs
              _ -> throwErrorHere $ TypeError $ "drop: second argument must be a list of vector"
          _ -> throwErrorHere $ TypeError $ "drop: first argument must be a number"
      )
    , ( "take"
      , "Returns the first `n` items of a sequence, unless the sequence is too short,\
        \ in which case an exception is thrown."
      , twoArg "take" $ \case
          (Number n, vs) -> case Num.isInt n of
            Left _ -> throwErrorHere $ OtherError "take: first argument must be an integer"
            Right i -> case vs of
              List xs -> pure . List $ take i xs
              Vec xs -> pure . Vec $ Seq.take i xs
              _ -> throwErrorHere $ TypeError $ "take: second argument must be a list of vector"
          _ -> throwErrorHere $ TypeError $ "take: first argument must be a number"
      )
    , ( "nth"
      , "Given an integral number `n` and `xs`, returns the `n`th element\
        \ (zero indexed) of `xs` when `xs` is a list or a vector. If `xs`\
        \ does not have an `n`-th element, or if it is not a list or vector, then\
        \ an exception is thrown."
      , \case
          [Number n, vs] -> case Num.isInt n of
            Left _ -> throwErrorHere $ OtherError "nth: first argument was not an integer"
            Right i -> do
              xs <- hoistEitherWith (const (toLangError . OtherError $ "nth: first argument must be sequential")) $ fromRad vs
              case xs `atMay` i of
                Just x  -> pure x
                Nothing -> throwErrorHere $ OtherError "nth: index out of bounds"
          [_,_] -> throwErrorHere $ TypeError "nth: expects a integer and a list or vector"
          xs -> throwErrorHere $ WrongNumberOfArgs "nth" 2 (length xs)
      )
    , ( "sort-by"
      , "Given a sequence `xs` and a function `f`, returns a sequence with the same\
        \ elements `x` of `xs` but sorted according to `(f x)`."
      , twoArg "sort-by" $ \case
          (f, List xs) -> do ys <- traverse (\x -> (x,) <$> callFn f [x]) xs
                             let ys' = sortWith snd ys
                             pure (List (fst <$> ys'))
          (f, Vec xs) -> do ys <- traverse (\x -> (x,) <$> callFn f [x]) xs
                            let ys' = Seq.sortOn snd ys
                            pure (Vec (fst <$> ys'))
          _ -> throwErrorHere $ TypeError "sort-by: second argument must be a sequence"
      )

    -- Dicts
    , ("lookup"
      , "Given a value `k` (the 'key') and a dict `d`, returns the value associated\
        \ with `k` in `d`. If the key does not exist in `d` then `()` is returned\
        \ instead. If `d` is not a dict then an exception is thrown."
      , \case
          [a, Dict m] -> case Map.lookup a m of
              Just v  -> pure v
              Nothing -> throwErrorHere $ TypeError $ "lookup: key did not exist: " <> renderCompactPretty a
          [_, _]      -> throwErrorHere $ TypeError $ "lookup: second argument must be a dict"
          xs -> throwErrorHere $ WrongNumberOfArgs "lookup" 2 (length xs))
    , ( "map-values"
      , "Given a function `f` and a dict `d`, returns a dict with the same keys as `d`\
        \ but `f` applied to all the associated values."
      , twoArg "map-values" $ \case
          (f, Dict m) -> do
            let kvs = Map.toList m
            vs <- traverse (\v -> callFn f [v]) (snd <$> kvs)
            pure . Dict . Map.fromList $ zip (fst <$> kvs) vs
          _ -> throwErrorHere $ TypeError $ "map-values: second argument must be a dict"
      )

    -- Structures
    , ( "<>"
      , "Merges two structures together. On vectors this performs concatenations. On dicts this performs the right-biased merge."
      , twoArg "<>" $ \case
          (List xs, List ys) -> pure $ List (xs ++ ys)
          (Vec xs, Vec ys) -> pure $ Vec (xs Seq.>< ys)
          (Dict m, Dict n) -> pure $ Dict (n <> m)
          _ -> throwErrorHere $ TypeError "<>: only works on vectors of dicts"
      )

    , ( "string-length"
      , "Returns the length of a string."
      , oneArg "string-length" $ \case
          String s -> pure . Number . fromIntegral . T.length $ s
          _ -> throwErrorHere $ TypeError "string-length: expecting string"
      )
    , ( "string-append"
      , "Concatenates a variable number of string arguments. If one of the arguments\
        \ isn't a string then an exception is thrown."
      , \args ->
          let fromStr (String s) = Just s
              fromStr _          = Nothing
              ss = fromStr <$> args
          in if all isJust ss
              then pure . String . mconcat $ catMaybes ss
              else throwErrorHere $ TypeError "string-append: non-string argument"
      )
    , ( "insert"
      , "Given `k`, `v` and a dict `d`, returns a dict with the same associations\
        \ as `d` but with `k` associated to `d`. If `d` isn't a dict or if `k` isn't\
        \ hashable then an exception is thrown."
      , \case
          [k, v, Dict m] ->
            if hashable k
            then pure . Dict $ Map.insert k v m
            else throwErrorHere NonHashableKey
          [_, _, _]                -> throwErrorHere

                                      $ TypeError "insert: third argument must be a dict"
          xs -> throwErrorHere $ WrongNumberOfArgs "insert" 3 (length xs))
    , ( "delete"
      , "Given `k` and a dict `d`, returns a dict with the same associations as `d` but\
        \ without the key `k`. If `d` isn't a dict then an exception is thrown."
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
    , numBinop (+) "+" "Adds two numbers together."
    , numBinop (*) "*" "Multiplies two numbers together."
    , numBinop (-) "-" "Substracts one number from another."
    , ( "/"
      , "Divides one number by another. Throws an exception if the second argument is 0."
      , twoArg "/" $ \case
          (Number x, Number y) | y /= 0 -> pure $ Number (x / y)
          (Number _, Number _) -> throwErrorHere $ OtherError "Can't divide by 0"
          _ -> throwErrorHere $ TypeError "/: expects two numbers"
      )
    , ( "<"
      , "Checks if a number is strictly less than another."
      , \case
          [Number x, Number y] -> pure $ Boolean (x < y)
          [_, _]               -> throwErrorHere $ TypeError "<: expecting number"
          xs                   -> throwErrorHere $ WrongNumberOfArgs "<" 2 (length xs))
    , ( ">"
      , "Checks if a number is strictly greater than another."
      , \case
          [Number x, Number y] -> pure $ Boolean (x > y)
          [_, _]               -> throwErrorHere $ TypeError ">: expecting number"
          xs                   -> throwErrorHere $ WrongNumberOfArgs ">" 2 (length xs))
    , ( "integral?"
      , "Checks if a number is an integer."
      , oneArg "integral?" $ \case
          Number n -> case Num.isInteger n of
            Left _  -> pure $ Boolean False
            Right _ -> pure $ Boolean True
          _ -> throwErrorHere $ TypeError "integral?: expecting number"
      )
    , ( "foldl"
      , "Given a function `f`, an initial value `i` and a sequence (list or vector)\
        \ `xs`, reduces `xs` to a single value by starting with `i` and repetitively\
        \ combining values with `f`, using elements of `xs` from left to right."
      , \case
          [fn, init', v] -> do
            ls :: [Value] <- fromRadOtherErr v
            foldlM (\b a -> callFn fn [b, a]) init' ls
          xs                   -> throwErrorHere $ WrongNumberOfArgs "foldl" 3 (length xs))
    , ( "foldr"
      , "Given a function `f`, an initial value `i` and a sequence (list or vector)\
        \ `xs`, reduces `xs` to a single value by starting with `i` and repetitively\
        \ combining values with `f`, using elements of `xs` from right to left."
      , \case
          [fn, init', v] -> do
            ls :: [Value] <- fromRadOtherErr v
            foldrM (\b a -> callFn fn [b, a]) init' ls
          xs                   -> throwErrorHere $ WrongNumberOfArgs "foldr" 3 (length xs))
    , ( "map"
      , "Given a function `f` and a sequence (list or vector) `xs`, returns a sequence\
        \ of the same size and type as `xs` but with `f` applied to all the elements."
      , \case
          [fn, List ls] -> List <$> traverse (callFn fn) (pure <$> ls)
          [fn, Vec ls]  -> Vec <$> traverse (callFn fn) (pure <$> ls)
          [_, _]        -> throwErrorHere $ TypeError "map: second argument should be a list or vector"
          xs            -> throwErrorHere $ WrongNumberOfArgs "map" 3 (length xs))
    , ( "keyword?"
      , isTy "keyword"
      , oneArg "keyword?" $ \case
          Keyword _ -> pure tt
          _         -> pure ff)
    , ( "atom?"
      , isTy "atom"
      , oneArg "atom?" $ \case
                  Atom _ -> pure tt
                  _      -> pure ff)
    , ( "list?"
      , isTy "list"
      , oneArg "list?" $ \case
                  List _ -> pure tt
                  _      -> pure ff)
    , ( "dict?"
      , isTy "dict"
      , oneArg "dict?" $ \case
                  Dict _ -> pure tt
                  _      -> pure ff)
    , ( "type"
      , "Returns a keyword representing the type of the argument; one of:\
        \ `:atom`, `:keyword`, `:string`, `:number`, `:boolean`, `:list`,\
        \ `:vector`, `:function`, `:dict`, `:ref`, `:function`."
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
    , ( "string?"
      , isTy "string"
      , oneArg "string?" $ \case
          String _ -> pure tt
          _        -> pure ff)
    , ( "boolean?"
      , isTy "boolean"
      , oneArg "boolean?" $ \case
          Boolean _ -> pure tt
          _         -> pure ff)
    , ( "number?"
      , isTy "number"
      , oneArg "number?" $ \case
          Number _ -> pure tt
          _        -> pure ff)
    , ( "member?"
      , "Given `v` and structure `s`, checks if `x` exists in `s`. The structure `s`\
        \ may be a list, vector or dict. If it is a list or a vector, it checks if `v`\
        \ is one of the items. If `s` is a dict, it checks if `v` is one of the keys."
      , \case
          [x, List xs] -> pure . Boolean $ elem x xs
          [x, Vec xs]  -> pure . Boolean . isJust $ Seq.elemIndexL x xs
          [x, Dict m]  -> pure . Boolean $ Map.member x m
          [_, _]       -> throwErrorHere
                        $ TypeError "member?: second argument must be list"
          xs           -> throwErrorHere $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ( "ref"
      , "Creates a ref with the argument as the initial value."
      , oneArg "ref" newRef)
    , ( "read-ref"
      , "Returns the current value of a ref."
      , oneArg "read-ref" $ \case
          Ref ref -> readRef ref
          _       -> throwErrorHere $ TypeError "read-ref: argument must be a ref")
    , ( "write-ref"
      , "Given a reference `r` and a value `v`, updates the value stored in `r` to be\
        \ `v` and returns `v`."
      , \case
          [Ref (Reference x), v] -> do
              st <- get
              put $ st { bindingsRefs = IntMap.insert x v $ bindingsRefs st }
              pure v
          [_, _]                 -> throwErrorHere
                                  $ TypeError "write-ref: first argument must be a ref"
          xs                     -> throwErrorHere
                                  $ WrongNumberOfArgs "write-ref" 2 (length xs))
    , ( "show"
      , "Returns a string representing the argument value."
      , oneArg "show" (pure . String . renderPrettyDef))
    , ( "seq"
      , "Given a structure `s`, returns a sequence. Lists and vectors are returned\
        \ without modification while for dicts a vector of key-value-pairs is returned:\
        \ these are vectors of length 2 whose first item is a key and whose second item\
        \ is the associated value."
      , oneArg "seq" $
          \case
            x@(List _) -> pure x
            x@(Vec _) -> pure x
            Dict kvs -> pure . Vec . Seq.fromList $ [Vec (Seq.fromList [k, v]) | (k,v) <- Map.toList kvs ]
            _ -> throwErrorHere $ TypeError "seq: can only be used on a list, vector or dictionary"
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
      , \case
          [keyv, sigv, String msg] -> do
            key <- fromRadOtherErr keyv
            sig <- fromRadOtherErr sigv
            pure . Boolean $ verifySignature key sig msg
          [_, _, _] -> throwErrorHere $ OtherError "verify-signature: message must be a string"
          xs -> throwErrorHere $ WrongNumberOfArgs "verify-signature" 3 (length xs)
      )
    , ( "public-key?"
      , "Checks if a value represents a valid public key."
      , oneArg "public-key?" $
          \v -> case fromRad v of
                  Right (_ :: PublicKey) -> pure $ Boolean True
                  Left _                 -> pure $ Boolean False
      )
    , ( "uuid?"
      , "Checks if a string has the format of a UUID."
      , oneArg "uuid?" $ \case
          String t -> pure . Boolean . UUID.isUUID $ t
          _ -> throwErrorHere $ TypeError "uuid?: expects a string"
      )
    , ( "document"
      , "Used to add documentation to variables."
      , \case
          [Atom i, List _, String desc] -> do
            v <- lookupAtom i
            defineAtom i (Just desc) v
            pure nil
          [_,_,_] -> throwErrorHere $ OtherError "document: expects an atom, a list of argument docs, and a string."
          xs -> throwErrorHere $ WrongNumberOfArgs "document" 3 (length xs)
      )
    , ( "doc"
      , "Returns the documentation string for a variable. To print it instead, use `doc!`."
      , oneArg "doc" $ \case
          Atom i -> do d <- lookupAtomDoc i
                       pure . String $
                         fromMaybe ("No documentation found for " <> fromIdent i <> ".") d
          _ -> throwErrorHere $ OtherError "doc: expects an atom"
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

    numBinop :: (Rational -> Rational -> Rational)
             -> Text
             -> Text
             -> (Text, Text, [Value] -> Lang m Value)
    numBinop fn name doc = (name, doc, \case
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

readValues
    :: (MonadError (LangError Value) m)
    => Text
    -> m Value
readValues s = do
    let p = parseValues "[read-many-primop]" s
    case p of
      Right v -> pure . Vec $ Seq.fromList v
      Left e  -> throwErrorHere $ ThrownError (Ident "parse-error") (String . toS $ parseErrorPretty e)

allDocs :: [(Text, Text, a)] -> [(Ident, Maybe Text, a)]
allDocs = fmap $ \(x,y,z) -> (unsafeToIdent x, Just y, z)
