{-# LANGUAGE QuasiQuotes #-}
module Radicle.Tests where

import           Data.Either (isLeft)
import           Data.Functor.Foldable (Fix(..), unfix)
import           Data.Functor.Identity (runIdentity)
import           Data.List (isSuffixOf)
import           Data.Semigroup ((<>))
import           Data.String.Interpolate (i)
import           Data.String.QQ (s)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Void (Void)
import           GHC.Exts (fromList, toList)
import           System.Directory (getDirectoryContents)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (counterexample, testProperty, (==>))

import           Radicle
import           Radicle.Internal.Arbitrary ()
import           Radicle.Internal.Core (toIdent)
import           Radicle.Internal.TestCapabilities

import           Paths_radicle

test_eval :: [TestTree]
test_eval =
    [ testCase "Fails for undefined atoms" $
        [s|blah|] `failsWith` UnknownIdentifier (toIdent "blah")

    , testCase "Succeeds for defined atoms" $ do
        let prog = [s|
              (define rocky-clark "Steve Wozniak")
              rocky-clark
              |]
        prog `succeedsWith` String "Steve Wozniak"

    , testCase "'dict' creates a Dict with given key/vals" $ do
        let prog = [s|(dict why "not")|]
        prog `succeedsWith` Dict (fromList [(toIdent "why", String "not")])

    , testCase "'cons' conses an element" $ do
        let prog = [s|(cons #t (list #f))|]
        prog `succeedsWith` List [Boolean True, Boolean False]

    , testCase "'head' returns the first element of a list" $ do
        let prog = [s|(head (list #t #f))|]
        prog `succeedsWith` Boolean True

    , testCase "'tail' returns the tail of a list" $ do
        let prog = [s|(tail (list #t #f #t))|]
        prog `succeedsWith` List [Boolean False, Boolean True]

    , testProperty "'eq?' considers equal values equal" $ \(val :: Value Void) -> do
        let prog = [i|(eq? #{renderPrettyDef val} #{renderPrettyDef val})|]
            res  = runTest' $ T.pack prog
        counterexample prog $  isLeft res || res == Right (Boolean True)

    , testCase "'eq?' works for quoted values" $ do
        let prog = [s|(eq? 'hi 'hi)|]
        prog `succeedsWith` Boolean True

    , testProperty "'eq?' considers different values different"
                $ \(v1 :: Value Int, v2 :: Value Int) ->
                  v1 /= v2 ==> do
        -- We quote the values to prevent errors from being thrown
        let prog = [i|(eq? (quote #{renderPrettyDef v1})
                           (quote #{renderPrettyDef v2}))|]
            res  = runTest' $ T.pack prog
        -- Either evaluation failed or their equal.
        counterexample prog $ isLeft res || res == Right (Boolean False)

    , testCase "'member?' returns true if list contains element" $ do
        let prog = [s|(member? #t (list #f #t))|]
        prog `succeedsWith` Boolean True

    , testCase "'member?' returns false if list does not contain element" $ do
        let prog = [s|(member? "hi" (list #f #t))|]
        prog `succeedsWith` Boolean False

    , testCase "'lookup' returns value of key in map" $ do
        let prog = [s|(lookup 'key1 (dict key1 "a" key2 "b"))|]
        prog `succeedsWith` String "a"

    , testCase "'insert' updates the value of key in map" $ do
        let prog = [s|(lookup 'key1 (insert 'key1 "b" (dict key1 "a" key2 "b")))|]
        prog `succeedsWith` String "b"

    , testCase "'insert' insert the value of key in map" $ do
        let prog = [s|(lookup 'key1 (insert 'key1 "b" (dict)))|]
        prog `succeedsWith` String "b"

    , testProperty "'string-append' concatenates string" $ \ss -> do
        let args = T.unwords $ renderPrettyFix . String <$> ss
            prog = "(string-append " <> args <> ")"
            res  = runTest' prog
            expected = Right . String $ mconcat ss
            info = "Expected:\n" <> prettyEither expected <>
                   "Got:\n" <> prettyEither res
        counterexample (T.unpack info) $ res == expected

    , testCase "'foldl' foldls the list" $ do
        let prog = [s|(foldl - 0 (list 1 2 3))|]
        prog `succeedsWith` Number (-6)

    , testCase "'foldr' foldrs the list" $ do
        let prog = [s|(foldr - 0 (list 1 2 3))|]
        prog `succeedsWith` Number 2

    , testCase "'map' maps over the list" $ do
        let prog = [s|(map (lambda (x) (+ x 1)) (list 1 2))|]
        prog `succeedsWith` List [Number 2, Number 3]

    , testCase "'eval' evaluates the list" $ do
        let prog = [s|(eval (quote #t))|]
        prog `succeedsWith` Boolean True

    , testCase "'eval' only evaluates the first quote" $ do
        let prog1 = [s|(eval (quote (quote (+ 3 2))))|]
            prog2 = [s|(quote (+ 3 2))|]
            res1 = runTest' prog1
            res2 = runTest' prog2
        res1 @?= res2

    , testProperty "'eval' does not alter functions" $ \(_v :: Value Int) -> do
        let prog1 = [i| (eval (lambda () #{renderPrettyDef _v})) |]
            prog2 = [i| (lambda () #{renderPrettyDef _v}) |]
            res1 = runTest' $ T.pack prog1
            res2 = runTest' $ T.pack prog2
            info = "Expected:\n" <> prettyEither res2
                <> "\nGot:\n" <> prettyEither res1
        counterexample (T.unpack info) $ res1 == res2

    , testCase "lambdas work" $ do
        let prog = [s|((lambda (x) x) #t)|]
        prog `succeedsWith` Boolean True

    , testCase "lambda does not have access to future definitions (hyper static)" $ do
        let prog = [s|
              (define y "a")
              (define lam (lambda () y))
              (define y "b")
              (lam)
              |]
        prog `succeedsWith` String "a"

    , testCase "lambdas allow local definitions" $ do
        let prog = [s|
              ((lambda ()
                  (define y 3)
                  y))
              |]
        prog `succeedsWith` Number 3

    , testCase "lambdas throw WrongNumberOfArgs if given wrong number of args" $ do
        let prog = [s| ((lambda (x) 3)) |]
        prog `failsWith` WrongNumberOfArgs "lambda" 1 0

    , testCase "'quote' gets evaluated the right number of times" $ do
        let prog = [s|
              (define test (lambda (x) (eq? x 'hi)))
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

    , testCase "'string?' is true for strings" $ do
        let prog = [s|(string? "hi")|]
        prog `succeedsWith` Boolean True

    , testCase "'string?' is false for nonStrings" $ do
        let prog = [s|(string? #f)|]
        prog `succeedsWith` Boolean False

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

    , testCase "'+' sums the list of numbers" $ do
        let prog1 = [s|(+ 2 (+ 2 3))|]
        prog1 `succeedsWith` Number 7
        let prog2 = [s|(+ -1 -2 -3)|]
        prog2 `succeedsWith` Number (- 6)

    , testCase "'-' subtracts the list of numbers" $ do
        let prog1= [s|(- (+ 2 3) 1)|]
        prog1 `succeedsWith` Number 4
        let prog2 = [s|(- -1 -2 -3)|]
        prog2 `succeedsWith` Number 4

    , testCase "'*' multiplies the list of numbers" $ do
        let prog = [s|(* 2 3)|]
        prog `succeedsWith` Number 6

    , testProperty "'>' works" $ \(x, y) -> do
        let prog = [i|(> #{renderPrettyFix $ Number x} #{renderPrettyFix $ Number y})|]
            res  = runTest' $ T.pack prog
        counterexample prog $ res == Right (Boolean (x > y))

    , testProperty "'<' works" $ \(x, y) -> do
        let prog = [i|(< #{renderPrettyFix $ Number x} #{renderPrettyFix $ Number y})|]
            res  = runTest' $ T.pack prog
        counterexample prog $ res == Right (Boolean (x < y))

    , testCase "'define' fails when first arg is not an atom" $ do
        let prog = [s|(define "hi" "there")|]
        prog `failsWith` OtherError "define expects atom for first arg"

    , testCase "'catch' catches thrown exceptions" $ do
        let prog = [s|
            (catch (quote exc) (throw (quote exc) #t) (lambda (y) y))
            |]
        prog `succeedsWith` Boolean True

    , testCase "'catch' has the correct environment" $ do
        let prog = [s|
            (define t #t)
            (define f #f)
            (catch (quote exc) (throw (quote exc) f) (lambda (y) t))
            |]
        prog `succeedsWith` Boolean True

    , testCase "'catch 'any' catches any exception" $ do
        let prog = [s|
            (catch 'any (throw (quote exc) #f) (lambda (y) #t))
            |]
        prog `succeedsWith` Boolean True

    , testCase "evaluation can be redefined" $ do
        let prog = [s|
            (define eval (lambda (x) #f))
            #t
            |]
        prog `succeedsWith` Boolean False

    , testCase "'read-ref' returns the most recent value" $ do
        let prog = [s|
                (define x (ref 5))
                (write-ref x 6)
                (read-ref x)
                |]
            res = runTest' prog
        res @?= Right (Number 6)

    , testProperty "read-ref . ref == id" $ \v -> do
        let derefed = runTest' $ T.pack [i|(read-ref (ref #{renderPrettyFix v}))|]
            orig    = runTest' $ T.pack [i|#{renderPrettyFix v}|]
            info    = "Expected:\n" <> T.unpack (prettyEither orig)
                   <> "\nGot:\n" <> T.unpack (prettyEither derefed)
        counterexample info $ derefed == orig

    , testProperty "'show' works" $ \(x :: Value ()) -> do
        let prog = [i|(show #{renderPrettyDef x})|]
            res  = runTest' $ T.pack prog
            info = "Expected:\n" <> T.unpack (renderPrettyDef x)
                 <> "\nGot:\n" <> T.unpack (prettyEither res)
        counterexample info $ res == Right (String (renderPrettyDef x))
    ]
  where
    failsWith src err    = runTest' src @?= Left err
    succeedsWith src val = runTest' src @?= Right val

test_parser :: [TestTree]
test_parser =
    [ testCase "parses strings" $ do
        "\"hi\"" ~~> String "hi"

    , testCase "parses booleans" $ do
        "#t" ~~> Boolean True
        "#f" ~~> Boolean False

    , testCase "parses primops" $ do
        "boolean?" ~~> Primop (toIdent "boolean?")
        "base-eval" ~~> Primop (toIdent "base-eval")

    , testCase "parses identifiers" $ do
        "++" ~~> Atom (toIdent "++")
        "what?crazy!" ~~> Atom (toIdent "what?crazy!")

    , testCase "parses identifiers that have a primop as prefix" $ do
        "evaluate" ~~> Atom (toIdent "evaluate")

    , testCase "parses function application" $ do
        "(++)" ~~> List [Atom (toIdent "++")]
        "(++ \"merge\" \"d\")" ~~> List [Atom (toIdent "++"), String "merge", String "d"]

    , testCase "parses number" $ do
        "0.15" ~~> Number 0.15
        "2000" ~~> Number 2000

    , testCase "parses identifiers" $ do
        "++" ~~> Atom (toIdent "++")
        "what?crazy!" ~~> Atom (toIdent "what?crazy!")

    , testCase "parses identifiers that have a primop as prefix" $ do
        "evaluate" ~~> Atom (toIdent "evaluate")
    ]
  where
    x ~~> y = parseTest x @?= Right y

test_binding :: [TestTree]
test_binding =
    [ testCase "handles shadowing correctly" $ do
        [s|(((lambda (x) (lambda (x) x)) "inner") "outer")|] ~~> String "outer"
    ]
  where
    x ~~> y = runIdentity (interpret "test" x pureEnv) @?= Right y


test_pretty :: [TestTree]
test_pretty =
    [ testProperty "pretty . parse == identity" $ \(val :: Value (Fix Value))  ->
        let rendered = renderPrettyDef val
            actual = parseTest rendered
            original = removeEnv' val
            info = case actual of
              Left e -> "parse error in: " <> T.unpack rendered <> "\n"
                      <> e
              Right v -> "pretty: " <> T.unpack rendered <> "\n"
                      <> "reparsed: " <> show v <> "\n"
                      <> "original: " <> show original <> "\n"
        in counterexample info $ actual == Right original
    ]

test_env :: [TestTree]
test_env =
    [ testProperty "fromList . toList == identity" $ \(env' :: Env (Value Int)) ->
        fromList (toList env') == env'
    ]

test_repl_primops :: [TestTree]
test_repl_primops =
    [ testProperty "get-line! returns the input line" $ \(v :: Value Void) ->
        let prog = [i|(eq? (get-line!) (quote #{renderPrettyDef v}))|]
            res = run [renderPrettyDef v] $ T.pack prog
        in counterexample prog $ res == Right (Boolean True)

    , testCase "catch catches get-line errors" $ do
        let prog = [s|
                 (define repl (dict name "repl" getter get-line!))
                 (catch 'any
                        (subscribe-to! repl (lambda (x) (print! x)))
                        (lambda (x) "caught"))
                 |]
            input = ["\"blah"]
            res = run input prog
        res @?= Right (String "caught")
    ]
  where
    run stdin prog = fst $ runTestWith replBindings stdin prog


test_repl :: [TestTree]
test_repl =
    [ testCase "evaluates correctly" $ do
        let input = [ "((lambda (x) x) #t)" ]
            output = [ "#t" ]
        (_, result) <- runInRepl input
        result @?= output

    , testCase "handles env modifications" $ do
        let input = [ "(define id (lambda (x) x))"
                    , "(id #t)"
                    ]
            output = [ "()"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @?= output

    , testCase "handles 'eval' redefinition" $ do
        let input = [ "(define eval (lambda (x) #t))"
                    , "#f"
                    ]
            output = [ "()"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @?= output

    , testCase "(define eval (quote base-eval)) doesn't change things" $ do
        let input = [ "(define eval (quote base-eval))"
                    , "(define id (lambda (x) x))"
                    , "(id #t)"
                    ]
            output = [ "()"
                     , "()"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @?= output
    ]
    where
      getCfg = getDataFileName "repl/config.rad" >>= T.readFile
      -- The repl catches exceptions, including the "out of stdin" exception
      -- that occurs at the end of a session, so we take the 'init' of the
      -- result.
      runInRepl inp = fmap init <$> (runTestWith replBindings inp <$> getCfg)

-- Tests all radicle files 'repl' dir. These should use the 'should-be'
-- function to ensure they are in the proper format.
test_source_files :: IO TestTree
test_source_files = testGroup "Radicle source file tests" <$> do
    oneOf'Em <- getDataFileName "repl/config.rad"
    let dir = reverse $ drop (length ("config.rad" :: String)) $ reverse oneOf'Em
    files <- getDirectoryContents dir
    let radFiles = filter (".rad" `isSuffixOf`) files
    sequence $ radFiles <&> \file -> do
        contents <- T.readFile $ dir <> file
        let (_, out) = runTestWith replBindings [] contents
        let makeTest line = let (name, result) = T.span (/= '\'')
                                               $ T.drop 1
                                               $ T.dropWhile (/= '\'') line
                            in testCase (T.unpack name) $ result @?= "' succeeded\""
        pure $ testGroup file
            $ [ makeTest ln | ln <- out, "\"Test" `T.isPrefixOf` ln ]

-- * Utils

-- | Environments are neither printed nor parsed, but are generated by the
-- arbitrary instance.
removeEnv :: Value s -> Value s
removeEnv v = case v of
    List xs         -> List $ removeEnv <$> xs
    Dict m          -> Dict $ removeEnv <$> m
    Lambda ids bd _ -> Lambda ids (removeEnv <$> bd) Nothing
    x               -> x

removeEnv' :: Value (Fix Value) -> Value (Fix Value)
removeEnv' v = Fix . removeEnv' . unfix <$> removeEnv v

prettyEither :: Pretty s => Either (LangError (Value s)) (Value s) -> T.Text
prettyEither (Left e)  = "Error: " <> renderPrettyDef e
prettyEither (Right v) = renderPrettyDef v

renderPrettyFix :: Value (Fix Value) -> T.Text
renderPrettyFix = renderPrettyDef

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
