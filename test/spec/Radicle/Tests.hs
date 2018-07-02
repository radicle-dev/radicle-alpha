{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Tests where

import           Data.Data ((:~:)(Refl), Data, cast, eqT)
import           Data.Either (isLeft)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics (everywhere)
import           Data.Semigroup ((<>))
import           Data.String.Interpolate (i)
import           Data.String.QQ (s)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Exts (fromList, toList)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (counterexample, testProperty, (==>))

import           Radicle
import           Radicle.Internal.Arbitrary ()
import           Radicle.Internal.Core (toIdent)
import           Radicle.Internal.TestCapabilities

import           Paths_radicle -- generate by cabal via 'data-files'

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

    , testCase "'sorted-map' creates a SortedMap with given key/vals" $ do
        let prog = [s|(sorted-map why "not")|]
        prog `succeedsWith` SortedMap (fromList [(toIdent "why", String "not")])

    , testCase "'cons' conses an element" $ do
        let prog = [s|(cons #t (list #f))|]
        prog `succeedsWith` List [Boolean True, Boolean False]

    , testCase "'car' returns the first element of a list" $ do
        let prog = [s|(car (list #t #f))|]
        prog `succeedsWith` Boolean True

    , testCase "'cdr' returns the tail of a list" $ do
        let prog = [s|(cdr (list #t #f #t))|]
        prog `succeedsWith` List [Boolean False, Boolean True]

    , testProperty "'eq?' considers equal values equal" $ \(val :: Value) -> do
        let prog = [i|(eq? #{renderPrettyDef val} #{renderPrettyDef val})|]
            res  = runIdentity $ runLang pureEnv
                               $ interpretMany "(test)" $ T.pack prog
        -- Either evaluation failed or their equal.
        counterexample prog $ isLeft res || res == Right (Boolean True)

    , testProperty "'eq?' considers different values different"
                $ \(v1 :: Value, v2 :: Value) ->
                  v1 /= v2 ==> do
        -- We quote the values to prevent errors from being thrown
        let prog = [i|(eq? (quote #{renderPrettyDef v1})
                           (quote #{renderPrettyDef v2}))|]
            res  = runIdentity $ runLang pureEnv
                               $ interpretMany "(test)" $ T.pack prog
        -- Either evaluation failed or their equal.
        counterexample prog $ isLeft res || res == Right (Boolean False)

    , testCase "'member?' returns true if list contains element" $ do
        let prog = [s|(member? #t (list #f #t))|]
        prog `succeedsWith` Boolean True

    , testCase "'member?' returns false if list does not contain element" $ do
        let prog = [s|(member? "hi" (list #f #t))|]
        prog `succeedsWith` Boolean False

    , testCase "'lookup' returns value of key in map" $ do
        let prog = [s|(lookup 'key1 (sorted-map key1 "a" key2 "b"))|]
        prog `succeedsWith` String "a"

    , testCase "'insert' updates the value of key in map" $ do
        let prog = [s|(lookup 'key1 (insert 'key1 "b" (sorted-map key1 "a" key2 "b")))|]
        prog `succeedsWith` String "b"

    , testCase "'insert' insert the value of key in map" $ do
        let prog = [s|(lookup 'key1 (insert 'key1 "b" (sorted-map)))|]
        prog `succeedsWith` String "b"

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
        let prog = [i|(> #{renderPrettyDef $ Number x} #{renderPrettyDef $ Number y})|]
            res  = runIdentity $ runLang pureEnv
                               $ interpretMany "(test)" $ T.pack prog
        counterexample prog $ res == Right (Boolean (x > y))

    , testProperty "'<' works" $ \(x, y) -> do
        let prog = [i|(< #{renderPrettyDef $ Number x} #{renderPrettyDef $ Number y})|]
            res  = runIdentity $ runLang pureEnv
                               $ interpretMany "(test)" $ T.pack prog
        counterexample prog $ res == Right (Boolean (x < y))

    , testProperty "quasiquote . unquote == id" $ \(v :: Value) -> do
        -- Parse errors will differ, so we ignore them.
        let equalModuloParseErr x y = case (x, y) of
              (Left (ParseError _) , Left (ParseError _)) -> True
              _ -> x == y
            prog1 = [i|(quasiquote (unquote #{renderPrettyDef v}))|]
            prog2 = [i|#{renderPrettyDef v}|]
            run'  = runIdentity . runLang pureEnv
                  . interpretMany "(test)" . T.pack
            l = run' prog1
            r = run' prog2
            info = "Left:\n" <> prettyEither l <>
                   "\nRight:\n" <> prettyEither r
        counterexample (T.unpack info) $ l `equalModuloParseErr` r

    , testCase "'define' fails when first arg is not an atom" $ do
        let prog = [s|(define "hi" "there")|]
        prog `failsWith` OtherError "define expects atom for first arg"

    , testCase "evaluation can be redefined" $ do
        let prog = [s|
            (define eval (lambda (x) #f))
            #t
            |]
        prog `succeedsWith` Boolean False
    ]
  where
    run src              = runIdentity $ runLang pureEnv
                                       $ interpretMany "(test)" src
    failsWith src err    = run src @?= Left err
    succeedsWith src val = run src @?= Right val

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
        "(++)" ~~> (Atom (toIdent "++") $$ [])
        "(++ \"merge\" \"d\")" ~~> (Atom (toIdent "++") $$ [String "merge", String "d"])

    , testCase "parses number" $ do
        "0.15" ~~> Number 0.15
        "2000" ~~> Number 2000

    , testCase "parses identifiers" $ do
        "++" ~~> Atom (toIdent "++")
        "what?crazy!" ~~> Atom (toIdent "what?crazy!")

    , testCase "parses identifiers that have a primop as prefix" $ do
        "evaluate" ~~> Atom (toIdent "evaluate")

    , testCase "parses function application" $ do
        "(++)" ~~> (Atom (toIdent "++") $$ [])
        "(++ \"merge\" \"d\")" ~~> (Atom (toIdent "++") $$ [String "merge", String "d"])
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
    [ testProperty "pretty . parse == identity" $ \val ->
        let rendered = renderCompactPretty val
            actual = parseTest rendered
            original = removeEnv val
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
    [ testProperty "fromList . toList == identity" $ \(env' :: Env) ->
        fromList (toList env') == env'
    ]

test_repl_primops :: [TestTree]
test_repl_primops =
    [ testProperty "get-line! returns the input line" $ \(v :: Value) ->
        let res = run [renderCompactPretty v] [s|(get-line!)|]
            expected = Right $ List [removeEnv v]
            info = "Expected: " <> show expected <> "\nGot: " <> show res
        in counterexample info $ res == expected

    , testCase "'deref-all' gives the value of a binding at call site" $ do
        let prog = [s|
                (define x #f)
                (define y (ref x))
                (define x #t)
                (deref-all y)
                |]
            res = run [] prog
        res @?= Right (Boolean True)

    , testProperty "deref-all . ref == id" $ \(i', v) -> do
        let derefed = Primop (toIdent "deref-all") $$ [Ref i']
            atom = Atom i'
            run' p = runTestWith (addBinding i' v pureEnv) [] (eval p)
        run' derefed == run' atom
    ]
    where
      run inp prog = fst $ runTestWith replBindings inp
                         $ interpretMany "(test)" prog

test_repl :: [TestTree]
test_repl =
    [ testCase "evaluates correctly" $ do
        let input = [ "((lambda (x) x) #t)" ]
            output = [ "(list #t)" ]
        (_, result) <- runInRepl input
        result @?= output

    , testCase "handles env modifications" $ do
        let input = [ "(define id (lambda (x) x))"
                    , "(id #t)"
                    ]
            output = [ "(list (list))"
                     , "(list #t)"
                     ]
        (_, result) <- runInRepl input
        result @?= output

    , testCase "handles 'eval' redefinition" $ do
        let input = [ "(define eval (lambda (x) #t))"
                    , "#f"
                    ]
            output = [ "(list (list))"
                     , "#t"
                     ]
        (_, result) <- runInRepl input
        result @?= output

    , testCase "(define eval (quote base-eval)) doesn't change things" $ do
        let input = [ "(define eval (quote base-eval))"
                    , "(define id (lambda (x) x))"
                    , "(id #t)"
                    ]
            output = [ "(list (list))"
                     , "(list (list))"
                     , "(list #t)"
                     ]
        (_, result) <- runInRepl input
        result @?= output
    ]
    where
      getCfg = getDataFileName "repl/config.rad" >>= T.readFile
      runInRepl inp = runTestWith replBindings inp
                    . interpretMany "(test)" <$> getCfg

-- * Utils

-- | Environments are neither printed nor parsed, but are generated by the
-- arbitrary instance.
removeEnv :: Value -> Value
removeEnv = everywhere go
  where
    go :: forall b. Data b => (b -> b)
    go x = case (cast x, eqT :: Maybe (b :~: Value)) of
        (Just (Lambda arg body _), Just Refl) -> Lambda arg body Nothing
        _                                     -> x

prettyEither :: Either LangError Value -> T.Text
prettyEither (Left e)  = "Error: " <> renderPrettyDef e
prettyEither (Right v) = renderPrettyDef v

