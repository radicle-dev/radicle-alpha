{-# LANGUAGE QuasiQuotes #-}
module Radicle.Tests where

import           Protolude hiding (toList)

import           Codec.Serialise (Serialise, deserialise, serialise)
import qualified Data.Map.Strict as Map
import           Data.Scientific (Scientific)
import           Data.Sequence (Seq(..))
import           Data.String.Interpolate (i)
import           Data.String.QQ (s)
import qualified Data.Text as T
import           GHC.Exts (fromList, toList)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Process (CmdSpec(..), StdStream(..))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
                 (Arbitrary, counterexample, testProperty, (==>))
import qualified Text.Megaparsec.Pos as Par

import           Radicle
import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Arbitrary
import           Radicle.Internal.Core (asValue, noStack)
import           Radicle.Internal.Foo (Bar(..), Baz(..), Foo)
import           Radicle.Internal.TestCapabilities
import           Radicle.TH

test_eval :: [TestTree]
test_eval =
    [ testCase "Fails for undefined atoms" $
        [s|blah|] `failsWith` UnknownIdentifier [ident|blah|]

    , testCase "Keywords eval to themselves" $
        [s|:blah|] `succeedsWith` Keyword [ident|blah|]

    , testCase "Succeeds for defined atoms" $ do
        let prog = [s|
              (def rocky-clark "Steve Wozniak")
              rocky-clark
              |]
        prog `succeedsWith` String "Steve Wozniak"

    , testCase "vector syntax creates vectors" $ do
        "[]" `succeedsWith` Vec Empty
        "[1 2]" `succeedsWith` vec [int 1, int 2]
        "[1 :a [:b 2]]" `succeedsWith` vec [int 1, kw "a", vec [kw "b", int 2]]

    , testCase "vectors evaluate their elements" $
        "[(+ 1 2) (* 3 4)]" `succeedsWith` vec [int 3, int 12]

    , testCase "'dict' creates a Dict with given key/vals" $ do
        let prog1 = [s|(dict 'why "not")|]
        prog1 `succeedsWith` Dict (fromList [(Atom $ [ident|why|], String "not")])
        let prog2 = [s|(dict 3 1)|]
        prog2 `succeedsWith` Dict (fromList [(int 3, int 1)])

    , testCase "Dicts evaluate both keys are arguments" $ do
        "{(+ 1 1) (+ 1 1)}" `succeedsWith` Dict (fromList [(int 2, int 2)])
        "{(+ 1 1) :a (+ 0 2) :b}" `succeedsWith` Dict (fromList [(int 2, kw "a")])

    , testCase "'cons' conses an element" $ do
        let prog = [s|(cons #t (list #f))|]
        prog `succeedsWith` List [Boolean True, Boolean False]

    , testCase "'first' returns the first element of a list" $ do
        let prog = [s|(first (list #t #f))|]
        prog `succeedsWith` Boolean True

    , testCase "'rest' returns the tail of a list" $ do
        let prog = [s|(rest (list #t #f #t))|]
        prog `succeedsWith` List [Boolean False, Boolean True]

    , testCase "'drop' drops" $ do
        "(drop 1 (list #t #f #t))" `succeedsWith` List [Boolean False, Boolean True]
        "(drop 2 [1 2 3 4])" `succeedsWith` toRad [3 :: Int, 4]

    , testCase "'nth' extracts elements from lists and vectors" $ do
        "(nth 0 [0 1 2])" `succeedsWith` int 0
        "(nth 1 [0 1 2])" `succeedsWith` int 1
        "(nth 0 '(0 1 2))" `succeedsWith` int 0
        "(nth 1 '(0 1 2))" `succeedsWith` int 1

    , testCase "'take n' takes n elements of a sequence" $ do
        "(take 0 [0 1 2])" `succeedsWith` vec []
        "(take 2 [0 1 2])" `succeedsWith` vec [Number 0, Number 1]
        "(take 2 (list 0 1 2))" `succeedsWith` List [Number 0, Number 1]

    , testCase "'drop n' drops n elements of a sequence" $ do
        "(drop 0 [0 1 2])" `succeedsWith` vec [Number 0, Number 1, Number 2]
        "(drop 2 [0 1 2])" `succeedsWith` vec  [Number 2]
        "(drop 2 (list 0 1 2))" `succeedsWith` List [Number 2]

    , testProperty "'eq?' considers equal values equal" $ \(val :: Value) -> do
        let prog = [i|(eq? #{renderPrettyDef val} #{renderPrettyDef val})|]
            res  = runPureCode $ toS prog
        counterexample prog $  isLeft res || res == Right (Boolean True)

    , testCase "'eq?' works for quoted values" $ do
        let prog = [s|(eq? 'hi 'hi)|]
        prog `succeedsWith` Boolean True

    , testProperty "'eq?' considers different values different"
                $ \(v1 :: Value , v2 :: Value ) ->
                  v1 /= v2 ==> do
        -- We quote the values to prevent errors from being thrown
        let prog = [i|(eq? (quote #{renderPrettyDef v1})
                           (quote #{renderPrettyDef v2}))|]
            res  = runPureCode $ toS prog
        -- Either evaluation failed or their equal.
        counterexample prog $ isLeft res || res == Right (Boolean False)

    , testCase "'integral?' returns true for whole numbers and false fractional numbers" $ do
        "(integral? 0)" `succeedsWith` Boolean True
        "(integral? 1)" `succeedsWith` Boolean True
        "(integral? 1/3)" `succeedsWith` Boolean False

    , testCase "'member?' returns true if list contains element" $ do
        let prog = [s|(member? #t (list #f #t))|]
        prog `succeedsWith` Boolean True

    , testCase "'member?' returns false if list does not contain element" $ do
        let prog = [s|(member? "hi" (list #f #t))|]
        prog `succeedsWith` Boolean False

    , testCase "'lookup' returns value of key in map" $ do
        let prog1 = [s|(lookup 'key1 (dict 'key1 "a" 'key2 "b"))|]
        prog1 `succeedsWith` String "a"
        let prog2 = [s|(lookup 5 (dict 5 "a" 'key2 "b"))|]
        prog2 `succeedsWith` String "a"
        let prog3 = [s|(lookup '(2 3) (dict '(2 3) "a" 'key2 "b"))|]
        prog3 `succeedsWith` String "a"

    , testCase "'insert' updates the value of key in map" $ do
        let prog1 = [s|(lookup 'key1 (insert 'key1 "b" (dict 'key1 "a" 'key2 "b")))|]
        prog1 `succeedsWith` String "b"
        let prog2 = [s|(lookup 5 (insert 5 "b" (dict 5 "a" 'key2 "b")))|]
        prog2 `succeedsWith` String "b"
        let prog3 = [s|(lookup '(2 3) (insert '(2 3) "b" (dict '(2 3) "a" 'key2 "b")))|]
        prog3 `succeedsWith` String "b"

    , testCase "'insert' inserts the value of key in map" $ do
        let prog1 = [s|(lookup 'key1 (insert 'key1 "b" (dict)))|]
        prog1 `succeedsWith` String "b"
        let prog2 = [s|(lookup 5 (insert 5 "b" (dict)))|]
        prog2 `succeedsWith` String "b"
        let prog3 = [s|(lookup '(2 3) (insert '(2 3) "b" (dict)))|]
        prog3 `succeedsWith` String "b"

    , testCase "dict keys should be hashable" $ do
        let f x = case noStack (runPureCode x) of
                Left (NonHashableKey _) -> True @?= True
                _ -> assertFailure "Expected NonHashableKey"
        f "(insert (ref 0) 1 {})"
        f "{(ref 0) 1}"
        "(lookup :k {'(fn [x] y) 1 :k :v})" `succeedsWith` Keyword [ident|v|]
        f "(dict (ref 0) 1)"

    , testProperty "'string-append' concatenates string" $ \ss -> do
        let args = T.unwords $ renderPrettyDef . asValue . String <$> ss
            prog = "(string-append " <> args <> ")"
            res  = runPureCode prog
            expected = Right . String $ mconcat ss
            info = "Expected:\n" <> prettyEither expected <>
                   "Got:\n" <> prettyEither res
        counterexample (toS info) $ res == expected

    , testProperty "'string-length' returns length of a string" $ \x -> do
        let prog = "(string-length " <> renderPrettyDef (asValue (String x)) <> ")"
            res  = runPureCode prog
            expected = Right $ int $ fromIntegral $ T.length x
            info = "Expected:\n" <> prettyEither (Right (String x)) <>
                   "Got:\n" <> prettyEither (runPureCode prog)
        counterexample (toS info) $ res == expected

    , testCase "'foldl' foldls the list" $ do
        let prog = [s|(foldl (fn [x y] (- x y)) 0 (list 1 2 3))|]
        prog `succeedsWith` int (-6)

    , testCase "'foldr' foldrs the list" $ do
        let prog = [s|(foldr (fn [x y] (- x y)) 0 (list 1 2 3))|]
        prog `succeedsWith` int 2

    , testCase "'foldl-string' foldls a string" $ do
        let prog = [s|(foldl-string (fn [x y] (cond (eq? y "a") (+ x 1) :else x))
                                    0
                                    "blablabla")
                   |]
        prog `succeedsWith` int 3

    , testCase "'map' maps over the list" $ do
        let prog = [s|(map (fn [x] (+ x 1)) (list 1 2))|]
        prog `succeedsWith` List [int 2, int 3]

    , testCase "'map' (and co.) don't over-eval elements of argument list" $ do
        let prog = [s|(map (fn [x] (cons 1 x)) (list (list 1)))
                   |]
        prog `succeedsWith` List [List [int 1, int 1]]

    , testCase "'eval' evaluates the list" $ do
        let prog = [s|(first (eval (quote #t) (get-current-state)))|]
        prog `succeedsWith` Boolean True

    , testCase "'eval' only evaluates the first quote" $ do
        let prog1 = [s|(first (eval (quote (quote (+ 3 2))) (get-current-state)))|]
            prog2 = [s|(quote (+ 3 2))|]
            res1 = runPureCode prog1
            res2 = runPureCode prog2
        res1 @?= res2

    , testProperty "'eval' does not alter functions" $ \(_v :: Value) -> do
        let prog1 = [i| (first (eval (fn [] #{renderPrettyDef _v}) (get-current-state))) |]
            prog2 = [i| (fn [] #{renderPrettyDef _v}) |]
            res1 = runPureCode $ toS prog1
            res2 = runPureCode $ toS prog2
            info = "Expected:\n" <> prettyEither res2
                <> "\nGot:\n" <> prettyEither res1
        counterexample (toS info) $ res1 == res2

    , testCase "lambdas with explicit arguments work" $ do
        let prog = [s|((fn [x] x) #t)|]
        prog `succeedsWith` Boolean True

    , testCase "lambdas with a vector of arguments work" $ do
        let prog = [s|((fn x (first x)) #t)|]
        prog `succeedsWith` Boolean True

    , testCase "lambda does not have access to future definitions (hyper static)" $ do
        let prog = [s|
              (def y "a")
              (def lam (fn [] y))
              (def y "b")
              (lam)
              |]
        prog `succeedsWith` String "a"

    , testCase "lambdas allow local definitions" $ do
        let prog = [s|
              ((fn []
                  (def y 3)
                  y))
              |]
        prog `succeedsWith` int 3

    , testCase "lambdas throw WrongNumberOfArgs if given wrong number of args" $ do
        let prog = [s| ((fn [x] 3)) |]
        prog `failsWith` WrongNumberOfArgs "lambda" 1 0

    , testCase "'quote' gets evaluated the right number of times" $ do
        let prog = [s|
              (def test (fn [x] (eq? x 'hi)))
              (test 'hi)
              |]
        prog `succeedsWith` Boolean True

    , testCase "'cond' works for first condition" $ do
        let prog = [s|(cond #t "a" :else "b")|]
        prog `succeedsWith` String "a"

    , testCase "'cond' works for second condition" $ do
        let prog = [s|(cond #f "a" #t "b")|]
        prog `succeedsWith` String "b"

    , testCase "'cond' is lazy" $ do
        let prog = [s|(cond #t "a" :else (#t "non-sense"))|]
        prog `succeedsWith` String "a"

    , testCase "'keyword?' is true for keywords" $ do
        "(keyword? :foo)" `succeedsWith` Boolean True

    , testCase "'keyword?' is false for non keywords" $ do
        "(keyword? #t)" `succeedsWith` Boolean False

    , testCase "'do' returns the empty list if called on nothing" $ do
        "(do)" `succeedsWith` List []

    , testCase "'do' returns the result of the last argument" $ do
        "(do 1)" `succeedsWith` int 1
        "(do 1 2 3)" `succeedsWith` int 3

    , testCase "'do' runs effects in order" $ do
        let prog = [s|
                   (def r (ref 0))
                   (do (write-ref r 1)
                       (write-ref r 2))
                   (read-ref r)
                   |]
        prog `succeedsWith` int 2

    , testCase "'string?' is true for strings" $ do
        let prog = [s|(string? "hi")|]
        prog `succeedsWith` Boolean True

    , testCase "'string?' is false for nonStrings" $ do
        let prog = [s|(string? #f)|]
        prog `succeedsWith` Boolean False

    , testCase "'boolean?' is true for booleans" $ do
        let prog = [s|(boolean? #t)|]
        prog `succeedsWith` Boolean True

    , testCase "'boolean?' is false for non-booleans" $ do
        let prog = [s|(boolean? "hi")|]
        prog `succeedsWith` Boolean False

    , testCase "'number?' is true for numbers" $ do
        let prog = [s|(number? 200)|]
        prog `succeedsWith` Boolean True

    , testCase "'number?' is false for non-numbers" $ do
        let prog = [s|(number? #t)|]
        prog `succeedsWith` Boolean False

    , testCase "'list?' works" $ do
        "(list? '(1 :foo ))" `succeedsWith` Boolean True
        "(list? '())" `succeedsWith` Boolean True
        "(list? :foo)" `succeedsWith` Boolean False

    , testCase "'dict?' works" $ do
        "(dict? (dict))" `succeedsWith` Boolean True
        "(dict? (dict :key 2))" `succeedsWith` Boolean True
        "(dict? :foo)" `succeedsWith` Boolean False

    , testCase "'type' returns the type, as a keyword" $ do
        let hasTy prog ty = prog `succeedsWith` kw ty
        "(type :keyword)" `hasTy` "keyword"
        "(type \"string\")" `hasTy` "string"
        "(type 'a)" `hasTy` "atom"
        "(type 1)" `hasTy` "number"
        "(type #t)" `hasTy` "boolean"
        "(type list?)" `hasTy` "function"
        "(type (list 1 2 3))" `hasTy` "list"
        "(type (dict 1 2))" `hasTy` "dict"
        "(type (ref 0))" `hasTy` "ref"
        "(type (fn [x] x))" `hasTy` "function"

    , testCase "'+' sums two numbers" $ do
        let prog1 = [s|(+ 2 1)|]
        prog1 `succeedsWith` int 3
        let prog2 = [s|(+ -1 -2)|]
        prog2 `succeedsWith` int (- 3)

    , testCase "'-' subtracts two numbers" $ do
        let prog1= [s|(- 5 1)|]
        prog1 `succeedsWith` int 4
        let prog2 = [s|(- 6 -3)|]
        prog2 `succeedsWith` int 9

    , testCase "'*' multiplies the list of numbers" $ do
        let prog = [s|(* 2 3)|]
        prog `succeedsWith` int 6

    , testProperty "'>' works" $ \(x, y) -> do
        let prog = [i|(> #{renderPrettyDef . asValue $ Number x} #{renderPrettyDef . asValue $ Number y})|]
            res  = runPureCode $ toS prog
        counterexample prog $ res == Right (Boolean (x > y))

    , testProperty "'<' works" $ \(x, y) -> do
        let prog = [i|(< #{renderPrettyDef . asValue $ Number x} #{renderPrettyDef . asValue $ Number y})|]
            res  = runPureCode $ toS prog
        counterexample prog $ res == Right (Boolean (x < y))

    , testCase "'def' fails when first arg is not an atom" $ do
        let prog = [s|(def "hi" "there")|]
        prog `failsWith` OtherError "def expects atom for first arg"

    , testCase "'catch' catches thrown exceptions" $ do
        let prog = [s|
            (catch (quote exc) (throw (quote exc) #t) (fn [y] y))
            |]
        prog `succeedsWith` Boolean True

    , testCase "'catch' has the correct environment" $ do
        let prog = [s|
            (def t #t)
            (def f #f)
            (catch (quote exc) (throw (quote exc) f) (fn [y] t))
            |]
        prog `succeedsWith` Boolean True

    , testCase "'catch 'any' catches any exception" $ do
        let prog = [s|
            (catch 'any (throw (quote exc) #f) (fn [y] #t))
            |]
        prog `succeedsWith` Boolean True

    , testCase "tx can be redefined" $ do
        let prog = [s|
            (def tx (fn [expr] #f))
            #t
            |]
        prog `succeedsWith` Boolean False

    , testCase "redefining eval keeps access to future definitions" $ do
        let prog = [s|
            (def eval (fn [expr env] (eval expr env)))
            (def t #t)
            t
            |]
        prog `succeedsWith` Boolean True

    , testCase "'read-ref' returns the most recent value" $ do
        let prog = [s|
                (def x (ref 5))
                (write-ref x 6)
                (read-ref x)
                |]
            res = runPureCode prog
        res @?= Right (int 6)

    , testCase "mutations to refs are persisted beyond a lambda's scope" $ do
        let prog = [s|
            (def inc-ref
              (fn [r]
                (def temp (read-ref r))
                (write-ref r (+ temp 1))
                ))
            (def a (ref 0))
            (inc-ref a)
            (read-ref a)
            |]
        let prog2 = [s|
            (def ref-mapper
              (fn [f]
                (fn [r]
                  (def temp (read-ref r))
                  (write-ref r (f temp)))))
            (def ref-incer (ref-mapper (fn [x] (+ x 1))))
            (def foo (ref 0))
            (ref-incer foo)
            (read-ref foo)
            |]
        runPureCode prog @?= Right (int 1)
        runPureCode prog2 @?= Right (int 1)

    , testCase "muliple refs mutated in multiple lambdas" $ do
        let prog = [s|
            (def make-counter
              (fn []
                (def current (ref 0))
                (fn []
                  (def temp (read-ref current))
                  (write-ref current (+ temp 1))
                  temp)))
            (def c1 (make-counter))
            (c1)
            (c1)
            (def c2 (make-counter))
            (c2)
            (c1)
            (list (c1) (c2))
            |]
        runPureCode prog @?= Right (List [int 3, int 1])

    , testProperty "read-ref . ref == id" $ \(v :: Value) -> do
        let derefed = runPureCode $ toS [i|(read-ref (ref #{renderPrettyDef v}))|]
            orig    = runPureCode $ toS [i|#{renderPrettyDef v}|]
            info    = "Expected:\n" <> toS (prettyEither orig)
                   <> "\nGot:\n" <> toS (prettyEither derefed)
        counterexample info $ noStack derefed == noStack orig

    , testCase "'show' works" $ do
        runPureCode "(show 'a)" @?= Right (String "a")
        runPureCode "(show ''a)" @?= Right (String "(quote a)")
        runPureCode "(show \"hello\")" @?= Right (String "\"hello\"")
        runPureCode "(show 42)" @?= Right (String "42")
        runPureCode "(show 1/3)" @?= Right (String "1/3")
        runPureCode "(show #t)" @?= Right (String "#t")
        runPureCode "(show #f)" @?= Right (String "#f")
        runPureCode "(show (list 'a 1 \"foo\" (list 'b ''x 2 \"bar\")))" @?= Right (String "(a 1 \"foo\" (b (quote x) 2 \"bar\"))")
        runPureCode "(show [1 :a])" @?= Right (String "[1 :a]")
        runPureCode "tx" @?= Right (PrimFn [ident|initial-tx|])
        runPureCode "(show (dict 'a 1))" @?= Right (String "{a 1}")
        runPureCode "(show (fn [x] x))" @?= Right (String "(fn [x] x)")

    , testCase "'show-unbounds' works" $ do
        runPureCode "(show-unbound {:b 2 :a 1})" @?= Right (String "{:a 1 :b 2}")
        runPureCode "(show-unbound {:bar \"bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar\" :foo \"foo foo foo foo foo\"})"
          @?= Right (String "{:bar \"bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar\" :foo \"foo foo foo foo foo\"}")

    , testProperty "'show-unbounds does not add any whitespace characters'" $ \(NoSpaceValue val :: NoSpaceValue) -> do
        let res = runPureCode $ toS [i|(show-unbound (quote #{renderPrettyUnbounded val}))|]
            info = "Expected to not include any double whitespace characters, line breaks or tabs:\n" <> toS (prettyEither res)
        counterexample info $ no_unallowed_whitespace (prettyEither res)

    , testCase "'read-anotated' works" $
        runPureCode "(read-annotated \"foo\" \"(:hello 42)\")" @?= Right (List [Keyword [ident|hello|], int 42])

    , testCase "'to-json' works" $ do
        runPureCode "(to-json (dict \"foo\" #t))" @?= Right (String "{\"foo\":true}")
        runPureCode "(to-json {:foo #t \"bar\" #f})" @?= Right (String "{\"foo\":true,\"bar\":false}")
        runPureCode "(to-json (dict \"key\" (list 1 \"value\")))" @?= Right (String "{\"key\":[1,\"value\"]}")
        runPureCode "(to-json [3/2 #f])" @?= Right (String "[1.5,false]")
        runPureCode "(to-json {1 2})" @?= Right (String "{\"1\":2}")
        runPureCode "(to-json {#t #f})" @?= Right (String "{\"true\":false}")
        noStack (runPureCode "(to-json {[1] 2})") @?= Left (OtherError "Could not convert to JSON: Cannot convert values of type Vec to a JSON key")

    , testCase "def-rec can define recursive functions" $ do
        let prog = [s|
            (def-rec triangular
              (fn [n]
                (cond
                  (eq? n 0) 0
                  :else     (+ n (triangular (- n 1))))))
            (triangular 10)
            |]
        runPureCode prog @?= Right (int 55)
    , testCase "def-rec shadows previous definition" $ do
        let prog = [s|
            (def decrement #f)
            (def-rec decrement
              (fn [n]
                (cond
                  (eq? n 0) 0
                  :else     (decrement (- n 1)))))
            (decrement 10)
            |]
        runPureCode prog @?= Right (int 0)

    , testCase "def-rec errors when defining a non-function" $
        failsWith "(def-rec x 42)" (OtherError "'def-rec' can only be used to define functions")

    , testCase "stack traces work" $ do
        let prog = [s|
            (def inner (fn [] (notdefined)))
            (def outer (fn [] (inner)))
            (outer)
            |]
        case runPureCode prog of
            Left (LangError stack (UnknownIdentifier (Identifier "notdefined"))) ->
                assertEqual "correct line numbers" [3,2,1,1] (stackTraceLines stack)
            r -> assertFailure $ "Didn't fail the way we expected: " ++ show r

    , testCase "stack traces work with HOFs" $ do
        let prog = [s|
            (def callit (fn [f] (f)))
            (def inner (fn [] (notdefined)))
            (callit inner)
            |]
        case runPureCode prog of
            Left (LangError stack (UnknownIdentifier (Identifier "notdefined"))) ->
                assertEqual "correct line numbers" [3,1,2,2] (stackTraceLines stack)
            r -> assertFailure $ "Didn't fail the way we expected: " ++ show r

    , testCase "Modules work" $ do
        let prog = [s|(module
                        {:module 'foo :doc "" :exports '[z]}
                        (def x 1)
                        (def y 2)
                        (def z (+ x y)))
                      (import foo)
                      (import foo :as 'bar)
                      (import foo ['z] :as 'baz)
                      (import foo :unqualified)
                      (list foo/z bar/z baz/z z)|]
        prog `succeedsWith` List (replicate 4 (Number 3))

    , testCase "Modules don't leak non-exported defs" $ do
        let prog = [s|(module
                        {:module 'foo :doc "" :exports '[z]}
                        (def x 1)
                        (def y 2)
                        (def z (+ x y)))
                      (import foo :unqualified)
                      x|]
        prog `failsWith` UnknownIdentifier [ident|x|]

    , testCase "Modules complain if exports are undefined" $ do
        let prog = [s|(module {:module 'foo :doc "" :exports '[x y]} (def x 0))|]
        prog `failsWith` ModuleError (UndefinedExports [ident|foo|] [[ident|y|]])

    , testCase "Importing complains if symbols are not exported" $ do
        let prog = [s|(module
                        {:module 'foo :doc "" :exports '[x]}
                        (def x 1) ;; x: defined and exported
                        (def y 2) ;; y: defined but not exported
                                  ;; z: neither defined nor exported
                      )
                      (import foo '[y x z]) ;; failed imports appear sorted in error message
                      |]
        prog `failsWith` OtherError "import: cannot import undefined symbols: y, z"
    ]
  where
    failsWith src err    = noStack (runPureCode src) @?= Left err
    succeedsWith src val = runPureCode src @?= Right val
    no_tabs t = not $ T.any (== '\t') t
    no_line_breaks t = not $ T.any (== '\n') t
    no_double_spaces t = not $ T.isInfixOf "  " t
    no_unallowed_whitespace t = no_double_spaces t && no_line_breaks t && no_tabs t

stackTraceLines :: [Ann.SrcPos] -> [Int]
stackTraceLines = concatMap go
    where
    go Ann.InternalPos{} = []
    go (Ann.SrcPos pos)  = [Par.unPos (Par.sourceLine pos)]

test_parser :: [TestTree]
test_parser =
    [ testCase "parses strings" $ do
        "\"hi\"" ~~> String "hi"

    , testCase "parses booleans" $ do
        "#t" ~~> Boolean True
        "#f" ~~> Boolean False

    , testCase "parses keywords" $ do
        ":foo" ~~> kw "foo"
        ":what?crazy!" ~~> kw "what?crazy!"
        ":::" ~~> kw "::"
        "::foo" ~~> kw ":foo"
        ":456" ~~> kw "456"

    , testCase "parses identifiers" $ do
        "++" ~~> Atom [ident|++|]
        "what?crazy!" ~~> Atom [ident|what?crazy!|]

    , testCase "parses identifiers that have a primop as prefix" $ do
        "evaluate" ~~> Atom [ident|evaluate|]

    , testCase "parses function application" $ do
        "(++)" ~~> List [Atom [ident|++|]]
        "(++ \"merge\" \"d\")" ~~> List [Atom [ident|++|], String "merge", String "d"]

    , testCase "parses numbers" $ do
        "1/3" ~~> Number (1 % 3)
        "2000" ~~> Number 2000

    , testCase "parses identifiers" $ do
        "++" ~~> Atom [ident|++|]
        "what?crazy!" ~~> Atom [ident|what?crazy!|]

    , testCase "parses identifiers that have a primop as prefix" $ do
        "evaluate" ~~> Atom [ident|evaluate|]

    , testCase "parses dicts" $ do
        "{:foo 3}" ~~> Dict (Map.singleton (Keyword [ident|foo|]) (int 3))

    , testCase "parses short-lambdas" $ do
        "\\0" ~~> lam [] [int 0]
        "\\3/2" ~~> lam [] [Number (3 % 2)]
        "\\:foo" ~~> lam [] [kw "foo"]
        "\\\"foo\"" ~~> lam [] [String "foo"]
        "\\x" ~~> lam [] [atom "x"]
        "\\#t" ~~> lam [] [Boolean True]
        "\\?" ~~> lam ["?"] [atom "?"]
        "\\[]" ~~> lam [] [vec []]
        "\\[? 1]" ~~> lam ["?"] [vec [atom "?", int 1]]
        "\\[?1 ?2]" ~~> lam ["?1", "?2"] [vec [atom "?1", atom "?2"]]
        "\\{}" ~~> lam [] [Dict Map.empty]
        "\\{:a ?}" ~~> lam ["?"] [Dict (fromList [(kw "a", atom "?")])]
        "\\()" ~~> lam [] [List []]
        "\\(foo ? 42)" ~~> lam ["?"] [List [atom "foo", atom "?", int 42]]
        "\\(foo ?1 42 ?2)" ~~> lam ["?1", "?2"] [List [atom "foo", atom "?1", int 42, atom "?2"]]
        "\\(foo ?2 42 ?1)" ~~> lam ["?1", "?2"] [List [atom "foo", atom "?2", int 42, atom "?1"]]
        "\\(fn [x y] (foo x ? y))" ~~> lam ["?"] [lam ["x", "y"] [List [atom "foo", atom "x", atom "?", atom "y"]]]

    , testCase "short-lambdas only accepts ? or a mix of ?i where i in {1,...,9}" $ do
        assertBool "? and ?1" (isLeft $ parseTest "\\[? ?1]")
        assertBool "? and ?2" (isLeft $ parseTest "\\[? ?2]")
        assertBool "?0" (isLeft $ parseTest "\\(foo ?0)")
        assertBool "?42" (isLeft $ parseTest "\\[?42]")

    , testCase "short-lambdas cannot be nested" $ do
        assertBool "directly" (isLeft $ parseTest "\\\\?")
        assertBool "directly" (isLeft $ parseTest "\\\\(foo ?)")
        assertBool "directly" (isLeft $ parseTest "\\\\(foo ?1 ?2)")
        assertBool "indirectly" (isLeft $ parseTest "\\(foo \\(bar ?))")
        assertBool "indirectly" (isLeft $ parseTest "\\(foo \\(bar ?1 ?2))")

    , testCase "a dict litteral with an odd number of forms is a syntax error" $
        assertBool "succesfully parsed a dict with 3 forms" (isLeft $ parseTest "{:foo 3 4}")

    , testCase "mkIdent does not accept identifiers with numeric prefixes" $ do
        assertBool "not for -93G" (isNothing (mkIdent "-93G"))
        assertBool "not for +0X, no sir" (isNothing (mkIdent "+0X"))
        assertBool "5 is right out" (isNothing (mkIdent "5"))
    , testCase "mkIdent does not accept whitespaces" $ do
        assertBool "not for -93G" (isNothing (mkIdent "a b"))
    ]
  where
    (~~>) :: Text -> Value -> Assertion
    x ~~> y = (untag <$> parseTest x) @?= Right (untag y)
    lam args body = List $ [atom "fn", vec (atom <$> args)] ++ body

test_binding :: [TestTree]
test_binding =
    [ testCase "handles shadowing correctly" $ do
        [s|(((fn [x] (fn [x] x)) "inner") "outer")|] ~~> String "outer"
    ]
  where
    x ~~> y = runIdentity (interpret "test" x pureEnv) @?= Right y

test_pretty :: [TestTree]
test_pretty =
    [ testCase "long lists are indented" $ do
        let r = renderPretty (apl 5) (asValue (List [String "fn", String "abc"]))
        r @?= "(\"fn\"\n  \"abc\")"

    , testCase "lists try to fit on one line" $ do
        let r = renderPretty (apl 80) (asValue (List [atom "fn", atom "arg1", atom "arg2"]))
        r @?= "(fn arg1 arg2)"

    , testCase "dicts try to fit on one line" $ do
        let r = renderPretty (apl 80) (asValue (Dict $ Map.fromList [ (kw "k1", int 1)
                                                                    , (kw "k2", int 2)
                                                                    ]))
        r @?= "{:k1 1 :k2 2}"

    , testCase "dicts split with key-val pairs" $ do
        let r = renderPretty (apl 5) (asValue (Dict $ Map.fromList [ (kw "k1", int 1)
                                                                   , (kw "k2", int 2)
                                                                   ]))
        r @?= "{:k1 1\n :k2 2}"
    , testProperty "read . pprint == identity for strings" $ \(t :: Text) ->
        let v = asValue (String t)
            pp = renderPrettyDef v
            v_ = parseTest pp
        in v_ == Right v
    ]
  where
    apl cols = AvailablePerLine cols 1

test_env :: [TestTree]
test_env =
    [ testProperty "fromList . toList == identity" $ \(env' :: Env Value) ->
        fromList (toList env') == env'
    ]

test_repl_primops :: [TestTree]
test_repl_primops =
    [ testProperty "(read-annotated \"get-line!\" (get-line!)) returns the input line" $ \(v :: Value) ->
        let prog = [i|(eq? (read-annotated \"get-line!\" (get-line!)) (quote #{renderPrettyDef v}))|]
            -- We're not actually using IO
            res = unsafePerformIO $ run [renderPrettyDef v] $ toS prog
        in counterexample prog $ res == Right (Boolean True)

    , testCase "catch catches read-line errors" $ do
        let prog = [s|
                 (catch 'any (read-line!) (fn [x] "caught"))
                 |]
            input = ["\"blah"]
        res <- run input prog
        res @?= Right (String "caught")
    , testCase "read-file! can read a file" $ do
        let prog = "(read-file! \"foobar.rad\")"
            files = Map.singleton "foobar.rad" "foobar"
        res <- runCodeWithFiles files prog
        res @?= Right (String "foobar")
    , testCase "load! can load definitions" $ do
        let prog = [s|
                   (load! "foo.rad")
                   (+ foo bar)
                   |]
            files = Map.singleton "foo.rad" "(def foo 42) (def bar 8)"
        res <- runCodeWithFiles files prog
        res @?= Right (int 50)
    , testCase "load! ignores shebangs" $ do
        let prog = [s|
                   (load! "foo.rad")
                   taxi
                   |]
            files = Map.singleton "foo.rad" "#!/blah\n(def taxi 1797)"
        res <- runCodeWithFiles files prog
        res @?= Right (int 1797)
    , testCase "generating and verifying cryptographic signatures works" $ do
        let prog = [s|
                   (def my-keys (gen-key-pair! (default-ecc-curve)))
                   (def not-my-keys (gen-key-pair! (default-ecc-curve)))
                   (def sig (gen-signature! (lookup :private-key my-keys) "hello"))
                   (def tt (verify-signature (lookup :public-key my-keys) sig "hello"))
                   (def ff (verify-signature (lookup :public-key not-my-keys) sig "hello"))
                   (list tt ff)
                   |]
        res <- run [] prog
        res @?= Right (List [Boolean True, Boolean False])
    , testCase "uuid! generates a valid uuid" $ do
        let prog = [s| (uuid? (uuid!)) |]
        res <- run [] prog
        res @?= Right (Boolean True)

    , testCase "'exit!' throws an Exit error with the provided code" $ do
        let prog = [s|(exit! 5)|]
        Left (LangError _ errorData) <- run [] prog
        errorData @?= Exit 5
    ]
  where
    run stdin' prog =
        let ws = defaultWorldState { worldStateStdin = stdin' }
        in fst <$> runCodeWithWorld ws prog

test_repl :: [TestTree]
test_repl =
    [ testCase "evaluates correctly" $ do
        let input = [ "((fn [x] x) #t)" ]
            output = [ "#t" ]
        assertReplInteraction input output

    , testCase "handles env modifications" $ do
        let input = [ "(def id (fn [x] x))"
                    , "(id #t)"
                    ]
            output = [ "()"
                     , "#t"
                     ]
        assertReplInteraction input output

    , testCase "(def eval eval) doesn't change things" $ do
        let input = [ "(def eval eval)"
                    , "(def id (fn [x] x))"
                    , "(id #t)"
                    ]
            output = [ "()"
                     , "()"
                     , "#t"
                     ]
        assertReplInteraction input output

    , testCase "exceptions are non-fatal" $ do
        let input = [ "(throw 'something \"something happened\")"
                    , "#t"
                    ]
        assertReplInteraction input ["#t"]

    , testCase "load! a non-existent file is a non-fatal exception" $ do
        let input = [ "(load! \"not-a-thing.rad\")"
                    , "#t"
                    ]
        assertReplInteraction input ["#t"]
    ]

test_from_to_radicle :: [TestTree]
test_from_to_radicle =
    [ testGroup "()"
        [ testForType (Proxy :: Proxy ()) ]
    , testGroup "(Foo, Foo)"
        [ testForType (Proxy :: Proxy (Foo, Foo)) ]
    , testGroup "Scientific"
        [ testForType (Proxy :: Proxy Scientific) ]
    , testGroup "Text"
        [ testForType (Proxy :: Proxy Text) ]
    , testGroup "Maybe Text"
        [ testForType (Proxy :: Proxy (Maybe Text))]
    , testGroup "[Text]"
        [ testForType (Proxy :: Proxy [Text]) ]
    , testGroup "Generic a => a"
        [ testForType (Proxy :: Proxy Foo) ]
    , testGroup "Single constructor with selectors"
        [ testForType (Proxy :: Proxy Bar) ]
    , testGroup "Single constructor with no selectors"
        [ testForType (Proxy :: Proxy Baz) ]
    , testGroup "Constructors optional for single constructor datatypes"
        [ let v = Vec $ fromList [ kw "bar"
                                 , Dict $ fromList [ (kw "bar1", String "hello")
                                                   , (kw "bar2", int 42)
                                                   ]
                                 ]
              x = Bar{ bar1 = "hello", bar2 = 42 }
          in testCase "works with selectors" $ fromRad v @?= Right x
        , let v = Vec $ fromList [ kw "baz" , String "hello" ]
              x = Baz "hello"
          in testCase "works with no selectors" $ fromRad v @?= Right x
        ]

    , testGroup "StdStream"
        [ kw "inherit" ~~ Inherit
        , kw "create-pipe" ~~ CreatePipe
        , kw "no-stream" ~~ NoStream
        ]

    , testGroup "CmdSpec"
        [ vec [kw "shell", String "blah blah"] ~~ ShellCommand "blah blah"
        , vec  [kw "raw", String "blah", vec [String "blah", String "blah"]]
              ~~ RawCommand "blah" ["blah", "blah"]
        ]
    ]
  where
    (~~)
        :: (FromRad Ann.WithPos a, ToRad Ann.WithPos a, Eq a, Show a)
        => Value -> a -> TestTree
    v ~~ v' =
        testCase ("works on " <> show v') $ do
           fromRad v @?= Right v'
           untag (toRad v' :: Value) @?= untag v
    testForType
        :: forall a . (Arbitrary a, Show a, ToRad Ann.WithPos a, FromRad Ann.WithPos a, Eq a)
        => Proxy a -> TestTree
    testForType _ =
        testProperty "fromRadicle . toRadicle == id" $ \(v :: a ) -> do
            let expected = Right v
                got = fromRad (toRad v :: Value)
                info = "Expected\n\t" <> show expected
                    <> "\nGot\n\t" <> show got
            counterexample info $ got == expected

test_cbor :: [TestTree]
test_cbor =
    [ testGroup "UntaggedValue"
        [ testForType (Proxy :: Proxy UntaggedValue) ]
    , testGroup "Value"
        [ testForType (Proxy :: Proxy Value) ]
    ]
  where
    testForType
        :: forall a. (Arbitrary a, Show a, Serialise a, Eq a)
        => Proxy a -> TestTree
    testForType _ =
        testProperty "deserialise . serialise == id" $ \(v :: a) -> do
            let expected = v
                got = deserialise (serialise v)
                info = "Expected\n\t" <> show expected
                    <> "\nGot\n\t" <> show got
            counterexample info $ got == expected

-- * Utils

kw :: Text -> Value
kw = Keyword . unsafeToIdent

atom :: Text -> Value
atom = Atom . unsafeToIdent

int :: Integer -> Value
int = Number . fromIntegral

vec :: [Value] -> Value
vec = Vec . fromList

-- | Like 'parse', but uses "(test)" as the source name and the default set of
-- primops.
parseTest :: MonadError Text m => Text -> m Value
parseTest t = parse "(test)" t

prettyEither :: Either (LangError Value) Value -> T.Text
prettyEither (Left e)  = "Error: " <> renderPrettyDef e
prettyEither (Right v) = renderPrettyDef v

-- | Run a REPL with the given input lines and expected output.
--
-- We only assert the expected output against the end of the actual
-- output. This means that if @expected@ is @[a, b]@ then we only assert
-- that the last two lines of the actual output equal @[a, b]@.
assertReplInteraction :: [Text] -> [Text] -> IO ()
assertReplInteraction input expected = do
    (result, output) <- runCodeWithInput input "(load! (find-module-file! \"repl.rad\"))"
    case result of
        Left err -> assertFailure $ "Error thrown in Repl: " <> toS (renderPrettyDef err)
        Right _  -> pure ()
    -- In addition to the output of the lines tested, tests get
    -- printed, so we take only the last few output lines.
    reverse (take (length expected) output) @?= expected
