{-# LANGUAGE QuasiQuotes #-}
module Radicle.Tests where

import           Protolude hiding (toList)

import           Codec.Serialise (Serialise, deserialise, serialise)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Scientific (Scientific)
import           Data.Sequence (Seq(..))
import           Data.String.Interpolate (i)
import           Data.String.QQ (s)
import qualified Data.Text as T
import           GHC.Exts (fromList, toList)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
                 (Arbitrary, counterexample, testProperty, (==>))
import qualified Text.Megaparsec.Pos as Par

import           Radicle
import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Arbitrary ()
import           Radicle.Internal.Core (asValue, noStack)
import           Radicle.Internal.Foo (Foo)
import           Radicle.Internal.TestCapabilities

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
        "[1 2]" `succeedsWith` Vec (fromList [int 1, int 2])
        "[1 :a [:b 2]]" `succeedsWith` Vec (fromList [int 1, kw "a", Vec (fromList [kw "b", int 2])])

    , testCase "vectors evaluate their elements" $
        "[(+ 1 2) (* 3 4)]" `succeedsWith` Vec (fromList [int 3, int 12])

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

    , testCase "'head' returns the first element of a list" $ do
        let prog = [s|(head (list #t #f))|]
        prog `succeedsWith` Boolean True

    , testCase "'tail' returns the tail of a list" $ do
        let prog = [s|(tail (list #t #f #t))|]
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
        "(take 0 [0 1 2])" `succeedsWith` Vec (fromList [])
        "(take 2 [0 1 2])" `succeedsWith` Vec (fromList [Number 0, Number 1])
        "(take 2 (list 0 1 2))" `succeedsWith` List [Number 0, Number 1]

    , testCase "'drop n' drops n elements of a sequence" $ do
        "(drop 0 [0 1 2])" `succeedsWith` Vec (fromList [Number 0, Number 1, Number 2])
        "(drop 2 [0 1 2])" `succeedsWith` Vec (fromList [Number 2])
        "(drop 2 (list 0 1 2))" `succeedsWith` List [Number 2]

    , testProperty "'eq?' considers equal values equal" $ \(val :: Value) -> do
        let prog = [i|(eq? #{renderPrettyDef val} #{renderPrettyDef val})|]
            res  = runTest' $ toS prog
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
            res  = runTest' $ toS prog
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
        let f x = x `failsWith` NonHashableKey
        f "(insert (ref 0) 1 {})"
        f "{(ref 0) 1}"
        "(lookup :k {'(fn [x] y) 1 :k :v})" `succeedsWith` Keyword [ident|v|]
        f "(eval {'(fn [y] y) :a-fun} (get-current-env))"
        f "(dict (ref 0) 1)"

    , testProperty "'string-append' concatenates string" $ \ss -> do
        let args = T.unwords $ renderPrettyDef . asValue . String <$> ss
            prog = "(string-append " <> args <> ")"
            res  = runTest' prog
            expected = Right . String $ mconcat ss
            info = "Expected:\n" <> prettyEither expected <>
                   "Got:\n" <> prettyEither res
        counterexample (toS info) $ res == expected

    , testProperty "'string-length' returns length of a string" $ \x -> do
        let prog = "(string-length " <> renderPrettyDef (asValue (String x)) <> ")"
            res  = runTest' prog
            expected = Right $ int $ fromIntegral $ T.length x
            info = "Expected:\n" <> prettyEither (Right (String x)) <>
                   "Got:\n" <> prettyEither (runTest' prog)
        counterexample (toS info) $ res == expected

    , testCase "'foldl' foldls the list" $ do
        let prog = [s|(foldl (fn [x y] (- x y)) 0 (list 1 2 3))|]
        prog `succeedsWith` int (-6)

    , testCase "'foldr' foldrs the list" $ do
        let prog = [s|(foldr (fn [x y] (- x y)) 0 (list 1 2 3))|]
        prog `succeedsWith` int 2

    , testCase "'map' maps over the list" $ do
        let prog = [s|(map (fn [x] (+ x 1)) (list 1 2))|]
        prog `succeedsWith` List [int 2, int 3]

    , testCase "'map' (and co.) don't over-eval elements of argument list" $ do
        let prog = [s|(map (fn [x] (cons 1 x)) (list (list 1)))
                   |]
        prog `succeedsWith` List [List [int 1, int 1]]

    , testCase "'eval' evaluates the list" $ do
        let prog = [s|(head (eval (quote #t) (get-current-env)))|]
        prog `succeedsWith` Boolean True

    , testCase "'eval' only evaluates the first quote" $ do
        let prog1 = [s|(head (eval (quote (quote (+ 3 2))) (get-current-env)))|]
            prog2 = [s|(quote (+ 3 2))|]
            res1 = runTest' prog1
            res2 = runTest' prog2
        res1 @?= res2

    , testProperty "'eval' does not alter functions" $ \(_v :: Value) -> do
        let prog1 = [i| (head (eval (fn [] #{renderPrettyDef _v}) (get-current-env))) |]
            prog2 = [i| (fn [] #{renderPrettyDef _v}) |]
            res1 = runTest' $ toS prog1
            res2 = runTest' $ toS prog2
            info = "Expected:\n" <> prettyEither res2
                <> "\nGot:\n" <> prettyEither res1
        counterexample (toS info) $ res1 == res2

    , testCase "lambdas work" $ do
        let prog = [s|((fn [x] x) #t)|]
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

    , testCase "'if' works with three arguments and true cond" $ do
        let prog = [s|(if #t "a" "b")|]
        prog `succeedsWith` String "a"

    , testCase "'if' works with three arguments and false cond" $ do
        let prog = [s|(if #f "a" "b")|]
        prog `succeedsWith` String "b"

    , testCase "'if' is lazy" $ do
        let prog = [s|(if #t "a" (#t "non-sense"))|]
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

    , testCase "'+' sums the list of numbers" $ do
        let prog1 = [s|(+ 2 (+ 2 3))|]
        prog1 `succeedsWith` int 7
        let prog2 = [s|(+ -1 -2 -3)|]
        prog2 `succeedsWith` int (- 6)

    , testCase "'-' subtracts the list of numbers" $ do
        let prog1= [s|(- (+ 2 3) 1)|]
        prog1 `succeedsWith` int 4
        let prog2 = [s|(- -1 -2 -3)|]
        prog2 `succeedsWith` int 4

    , testCase "'*' multiplies the list of numbers" $ do
        let prog = [s|(* 2 3)|]
        prog `succeedsWith` int 6

    , testProperty "'>' works" $ \(x, y) -> do
        let prog = [i|(> #{renderPrettyDef . asValue $ Number x} #{renderPrettyDef . asValue $ Number y})|]
            res  = runTest' $ toS prog
        counterexample prog $ res == Right (Boolean (x > y))

    , testProperty "'<' works" $ \(x, y) -> do
        let prog = [i|(< #{renderPrettyDef . asValue $ Number x} #{renderPrettyDef . asValue $ Number y})|]
            res  = runTest' $ toS prog
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

    , testCase "evaluation can be redefined" $ do
        let prog = [s|
            (def eval (fn [expr env] (list #f env)))
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
            res = runTest' prog
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
        runTest' prog @?= Right (int 1)
        runTest' prog2 @?= Right (int 1)

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
        runTest' prog @?= Right (List [int 3, int 1])

    , testProperty "read-ref . ref == id" $ \(v :: Value) -> do
        let derefed = runTest' $ toS [i|(read-ref (ref #{renderPrettyDef v}))|]
            orig    = runTest' $ toS [i|#{renderPrettyDef v}|]
            info    = "Expected:\n" <> toS (prettyEither orig)
                   <> "\nGot:\n" <> toS (prettyEither derefed)
        counterexample info $ noStack derefed == noStack orig

    , testCase "'show' works" $ do
        runTest' "(show 'a)" @?= Right (String "a")
        runTest' "(show ''a)" @?= Right (String "(quote a)")
        runTest' "(show \"hello\")" @?= Right (String "\"hello\"")
        runTest' "(show 42)" @?= Right (String "42")
        runTest' "(show 1/3)" @?= Right (String "1/3")
        runTest' "(show #t)" @?= Right (String "#t")
        runTest' "(show #f)" @?= Right (String "#f")
        runTest' "(show (list 'a 1 \"foo\" (list 'b ''x 2 \"bar\")))" @?= Right (String "(a 1 \"foo\" (b (quote x) 2 \"bar\"))")
        runTest' "(show [1 :a])" @?= Right (String "[1 :a]")
        runTest' "eval" @?= Right (PrimFn [ident|base-eval|])
        runTest' "(show (dict 'a 1))" @?= Right (String "{a 1}")
        runTest' "(show (fn [x] x))" @?= Right (String "(fn [x] x)")

    , testCase "'read' works" $
        runTest' "(read \"(:hello 42)\")" @?= Right (List [Keyword [ident|hello|], int 42])

    , testCase "'to-json' works" $ do
        runTest' "(to-json (dict \"foo\" #t))" @?= Right (String "{\"foo\":true}")
        runTest' "(to-json (dict \"key\" (list 1 \"value\")))" @?= Right (String "{\"key\":[1,\"value\"]}")
        noStack (runTest' "(to-json (dict 1 2))") @?= Left (OtherError "Could not serialise value to JSON")

    , testCase "def-rec can define recursive functions" $ do
        let prog = [s|
            (def-rec triangular
              (fn [n]
                (if (eq? n 0)
                    0
                    (+ n (triangular (- n 1))))))
            (triangular 10)
            |]
        runTest' prog @?= Right (int 55)

    , testCase "def-rec errors when defining a non-function" $
        failsWith "(def-rec x 42)" (OtherError "def-rec can only be used to define functions")

    , testCase "stack traces work" $ do
        let prog = [s|
            (def inner (fn [] (notdefined)))
            (def outer (fn [] (inner)))
            (outer)
            |]
        case runTest' prog of
            Left (LangError stack (UnknownIdentifier (Identifier "notdefined"))) ->
                assertEqual "correct line numbers" [3,2,1,1] (stackTraceLines stack)
            r -> assertFailure $ "Didn't fail the way we expected: " ++ show r

    , testCase "stack traces work with HOFs" $ do
        let prog = [s|
            (def callit (fn [f] (f)))
            (def inner (fn [] (notdefined)))
            (callit inner)
            |]
        case runTest' prog of
            Left (LangError stack (UnknownIdentifier (Identifier "notdefined"))) ->
                assertEqual "correct line numbers" [3,1,2,2] (stackTraceLines stack)
            r -> assertFailure $ "Didn't fail the way we expected: " ++ show r
    ]
  where
    failsWith src err    = noStack (runTest' src) @?= Left err
    succeedsWith src val = runTest' src @?= Right val

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

    , testCase "a dict litteral with an odd number of forms is a syntax error" $
        assertBool "succesfully parsed a dict with 3 forms" (isLeft $ parseTest "{:foo 3 4}")

    , testCase "mkIdent does not accept identifiers with numeric prefixes" $ do
        assertBool "not for -93G" (isNothing (mkIdent "-93G"))
        assertBool "not for +0X, no sir" (isNothing (mkIdent "+0X"))
        assertBool "5 is right out" (isNothing (mkIdent "5"))
    ]
  where
    x ~~> y = parseTest x @?= Right y

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
    [ testProperty "(read (get-line!)) returns the input line" $ \(v :: Value) ->
        let prog = [i|(eq? (read (get-line!)) (quote #{renderPrettyDef v}))|]
            res = run [renderPrettyDef v] $ toS prog
        in counterexample prog $ res == Right (Boolean True)

    , testCase "catch catches read-line errors" $ do
        let prog = [s|
                 (catch 'any (read-line!) (fn [x] "caught"))
                 |]
            input = ["\"blah"]
            res = run input prog
        res @?= Right (String "caught")
    , testCase "read-file! can read a file" $ do
        let prog = "(read-file! \"foobar.rad\")"
            files = Map.singleton "foobar.rad" "foobar"
        runFiles files prog @?= Right (String "foobar")
    , testCase "load! can load definitions" $ do
        let prog = [s|
                   (load! "foo.rad")
                   (+ foo bar)
                   |]
            files = Map.singleton "foo.rad" "(def foo 42) (def bar 8)"
        runFiles files prog @?= Right (int 50)
    , testCase "generating and verifying cryptographic signatures works" $
        let prog = [s|
                   (def my-keys (gen-key-pair! (default-ecc-curve)))
                   (def not-my-keys (gen-key-pair! (default-ecc-curve)))
                   (def sig (gen-signature! (lookup :private-key my-keys) "hello"))
                   (def tt (verify-signature (lookup :public-key my-keys) sig "hello"))
                   (def ff (verify-signature (lookup :public-key not-my-keys) sig "hello"))
                   (list tt ff)
                   |]
        in run [] prog @?= Right (List [Boolean True, Boolean False])
    , testCase "uuid! generates a valid uuid" $
        let prog = [s| (uuid? (uuid!)) |]
        in run [] prog @?= Right (Boolean True)
    ]
  where
    run stdin' prog = fst $ runTestWith testBindings stdin' prog
    runFiles :: Map Text Text -> Text -> Either (LangError Value) Value
    runFiles files prog = fst $ runTestWithFiles testBindings [] files prog

test_repl :: [TestTree]
test_repl =
    [ testCase "evaluates correctly" $ do
        let input = [ "((fn [x] x) #t)" ]
            output = [ "#t" ]
        (_, result) <- runInRepl input
        result @==> output

    , testCase "handles env modifications" $ do
        let input = [ "(def id (fn [x] x))"
                    , "(id #t)"
                    ]
            output = [ "()"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @==> output

    , testCase "handles 'eval' redefinition" $ do
        let input = [ "(def eval (fn [expr env] (list #t env)))"
                    , "#f"
                    ]
            output = [ "()"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @==> output

    , testCase "(def eval base-eval) doesn't change things" $ do
        let input = [ "(def eval base-eval)"
                    , "(def id (fn [x] x))"
                    , "(id #t)"
                    ]
            output = [ "()"
                     , "()"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @==> output

    , testCase "exceptions are non-fatal" $ do
        let input = [ "(throw 'something \"something happened\")"
                    , "#t"
                    ]
        (_, result) <- runInRepl input
        result @==> ["#t"]

    , testCase "load! a non-existent file is a non-fatal exception" $ do
        let input = [ "(load! \"not-a-thing.rad\")"
                    , "#t"
                    ]
        (_, result) <- runInRepl input
        result @==> ["#t"]
    ]
    where
      -- In addition to the output of the lines tested, tests get
      -- printed, so we take only the last few output lines.
      r @==> out = reverse (take (length out) r) @?= out

test_from_to_radicle :: [TestTree]
test_from_to_radicle =
    [ testGroup "()"
        [ testForType (Proxy :: Proxy ()) ]
    , testGroup "(Foo, Foo)"
        [ testForType (Proxy :: Proxy (Foo, Foo)) ]
    , testGroup "Env Value"
        [ testForType (Proxy :: Proxy (Env Value)) ]
    , testGroup "Bindings ()"
        [ testForType (Proxy :: Proxy (Bindings ())) ]
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
    ]
  where
    testForType
        :: forall a. (Arbitrary a, Show a, ToRad Ann.WithPos a, FromRad Ann.WithPos a, Eq a)
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


-- Tests radicle files. These should use the ':test' macro to ensure
-- they are in the proper format.
-- Note that loaded files will also be tested, so there's no need to test files
-- loaded by the prelude individually.
test_source_files :: IO TestTree
test_source_files = do
    tests <- join <$> traverse testOne ["rad/prelude.rad", "rad/monadic/issues.rad"]
    pure $ testGroup "Radicle source file tests" tests
  where
    testOne :: FilePath -> IO [TestTree]
    testOne file = do
        (dir, files) <- sourceFiles
        allFiles <- forM files $ \f -> do
            contents <- readFile (dir <> f)
            pure (toS f, contents)
        contents <- readFile (dir <> file)
        let keyPair :: Text  = case runTest testBindings "(gen-key-pair! (default-ecc-curve))" of
                    Right kp -> renderCompactPretty kp
                    Left _ -> panic "Couldn't generate keypair file."
        let (r, out) = runTestWithFiles testBindings [] (Map.insert "my-keys.rad" keyPair (fromList allFiles)) contents
        let makeTest line =
                let name = T.reverse $ T.drop 1 $ T.dropWhile (/= '\'')
                         $ T.reverse $ T.drop 1 $ T.dropWhile (/= '\'') line
                in testCase (toS name) $
                    if "' succeeded\"" `T.isSuffixOf` line
                        then pure ()
                        else assertFailure . toS $ "test failed: " <> line
        let doesntThrow = if isRight r
                then pure ()
                else assertFailure $ "Expected Right, got: " <> toS (prettyEither r)
        pure $ [testGroup file
            $ testCase "doesn't throw" doesntThrow
            : [ makeTest ln | ln <- reverse out, "\"Test" `T.isPrefixOf` ln ]]

test_macros :: [TestTree]
test_macros =
    [ testCase ":enter-chain keeps old bindings" $ do
        let input = [ "(def x 0)"
                    , "(:enter-chain \"blah\")"
                    , "(def x 1)"
                    , ":quit"
                    , "x"
                    ]
            output = [ "0" ]
        (_, result) <- runInRepl input
        result @==> output
    ]
  where
    -- In addition to the output of the lines tested, 'should-be's get
    -- printed, so we take only the last few output lines.
    r @==> out = reverse (take (length out) r) @?= out


-- * Utils

kw :: Text -> Value
kw = Keyword . unsafeToIdent

atom :: Text -> Value
atom = Atom . unsafeToIdent

int :: Integer -> Value
int = Number . fromIntegral

-- -- | Like 'parse', but uses "(test)" as the source name and the default set of
-- -- primops.
parseTest :: MonadError Text m => Text -> m Value
parseTest t = parse "(test)" t

prettyEither :: Either (LangError Value) Value -> T.Text
prettyEither (Left e)  = "Error: " <> renderPrettyDef e
prettyEither (Right v) = renderPrettyDef v

-- Find repl.rad, give it all other files as possible imports, and run
-- it.
runInRepl :: [Text] -> IO (Either (LangError Value) Value, [Text])
runInRepl inp = do
        (dir, srcs) <- sourceFiles
        srcMap <- forM srcs (\src -> (T.pack src ,) <$> readFile (dir <> src))
        let replSrc = head [ src | (name, src) <- srcMap
                                 , "repl.rad" `T.isSuffixOf` name
                                 ]
        pure $ runTestWithFiles testBindings inp (Map.fromList srcMap) (fromJust replSrc)
