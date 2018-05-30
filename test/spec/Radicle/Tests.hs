{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Tests where

import           Data.Data ((:~:)(Refl), Data, cast, eqT)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics (everywhere)
import           Data.Semigroup ((<>))
import           Data.String.QQ (s)
import qualified Data.Text as T
import           GHC.Exts (fromList, toList)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (counterexample, testProperty)

import           Radicle.Internal.Arbitrary ()
import           Radicle.Internal.Core (identFromString)
import           Radicle.Internal.TestCapabilities

import           Radicle

test_eval :: [TestTree]
test_eval =
    [ testCase "Fails for undefined atoms" $
        [s|blah|] `failsWith` UnknownIdentifier (i "blah")

    , testCase "Succeeds for defined atoms" $ do
        let prog = [s|
              (define rocky-clark "Steve Wozniak")
              rocky-clark
              |]
        prog `succeedsWith` String "Steve Wozniak"

    , testCase "'sorted-map' creates a SortedMap with given key/vals" $ do
        let prog = [s|(sorted-map why "not")|]
        prog `succeedsWith` SortedMap (fromList [(i "why", String "not")])

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

    , testCase "'boolean?' is true for non-booleans" $ do
        let prog = [s|(boolean? "hi")|]
        prog `succeedsWith` Boolean False

    , testCase "'define' fails when first arg is not an atom" $ do
        let prog = [s|(define "hi" "there")|]
        prog `failsWith` OtherError "define expects atom for first arg"
    ]
  where
    run src              = runIdentity $ runLang pureEmptyEnv
                                       $ interpretMany "(test)" src
    failsWith src err    = run src @?= Left err
    succeedsWith src val = run src @?= Right val

test_parser :: [TestTree]
test_parser =
    [ testCase "parses strings" $ do
        "\"hi\"" ==> String "hi"

    , testCase "parses booleans" $ do
        "#t" ==> Boolean True
        "#f" ==> Boolean False

    , testCase "parses primops" $ do
        "boolean?" ==> Primop (i "boolean?")
        "eval" ==> Primop (i "eval")

    , testCase "parses identifiers" $ do
        "++" ==> Atom (i "++")
        "what?crazy!" ==> Atom (i "what?crazy!")

    , testCase "parses function application" $ do
        "(++)" ==> (Atom (i "++") $$ [])
        "(++ \"merge\" \"d\")" ==> (Atom (i "++") $$ [String "merge", String "d"])
    ]
  where
    x ==> y = parseTest x @?= Right y

test_binding :: [TestTree]
test_binding =
    [ testCase "handles shadowing correctly" $ do
        [s|(((lambda (x) (lambda (x) x)) "inner") "outer")|] ==> String "outer"
    ]
  where
    x ==> y = runIdentity (interpret "test" x pureEmptyEnv) @?= Right y


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
        let derefed = Primop (i "deref-all") $$ [Ref i']
            atom = Atom i'
            run' p = runTestWith (addBinding i' v pureEmptyEnv) [] (eval p)
        run' derefed == run' atom
    ]
    where
      run inp prog = fst $ runTestWith replBindings inp
                         $ interpretMany "(test)" prog


-- * Utils

i :: String -> Ident
i = identFromString

-- | Environments are neither printed nor parsed, but are generated by the
-- arbitrary instance.
removeEnv :: Value -> Value
removeEnv = everywhere go
  where
    go :: forall b. Data b => (b -> b)
    go x = case (cast x, eqT :: Maybe (b :~: Value)) of
        (Just (Lambda arg body _), Just Refl) -> Lambda arg body Nothing
        _                                     -> x
