{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Tests where

import           Data.Data ((:~:)(Refl), Data, cast, eqT, gmapT)
import           Data.Semigroup (Semigroup, (<>))
import           Data.String.QQ (s)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Exts (fromList, toList)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, counterexample)
import           Control.Monad.Morph (generalize, hoist)

import           Radicle.Internal.Arbitrary ()

import           Radicle

test_eval :: [TestTree]
test_eval =
    [ testCase "Fails for undefined atoms" $
        [s|blah|] `failsWith` UnknownIdentifier "blah"

    , testCase "Succeeds for defined atoms" $ do
        let prog = [s|
              (define rocky-clark "Steve Wozniak")
              rocky-clark
              |]
        prog `succeedsWith` String "Steve Wozniak"

    , testCase "'sorted-map' creates a SortedMap with given key/vals" $ do
        let prog = [s|(sorted-map why "not")|]
        prog `succeedsWith` SortedMap (fromList [("why", String "not")])

    , testCase "'eval' evaluates the list" $ do
        let prog = [s|(eval (quote #t))|]
        prog `succeedsWith` Boolean True

    , testCase "lambdas work" $ do
        let prog = [s|((lambda (x) x) #t)|]
        prog `succeedsWith` Boolean True

    , testCase "lambda does not have access to future definitions" $ do
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
    run :: Text -> Either LangError [Value]
    run src              = fmap snd $ foldChainFromSrc "test" src
                         $ hoist generalize genesisChain
    failsWith src err    = run src @?= Left err
    succeedsWith src val = (last <$> run src) @?= Right val

test_parser :: [TestTree]
test_parser =
    [ testCase "parses strings" $ do
        "\"hi\"" ==> String "hi"

    , testCase "parses booleans" $ do
        "#t" ==> Boolean True
        "#f" ==> Boolean False

    , testCase "parses primops" $ do
        "boolean?" ==> Primop "boolean?"
        "eval" ==> Primop "eval"

    , testCase "parses identifiers" $ do
        "++" ==> Atom "++"
        "what?crazy!" ==> Atom "what?crazy!"

    , testCase "parses function application" $ do
        "(++)" ==> (Atom "++" $$ [])
        "(++ \"merge\" \"d\")" ==> (Atom "++" $$ [String "merge", String "d"])
    ]
  where
    x ==> y = parseTest x @?= Right y

test_binding :: [TestTree]
test_binding =
    [ testCase "handles shadowing correctly" $ do
        [s|(((lambda (x) (lambda (x) x)) "inner") "outer")|] ==> String "outer"
    ]
  where
    x ==> y = interpret "test" x @?= Right y

test_subscriber :: [TestTree]
test_subscriber =
    [ testCase "have all effects returned in the right order" $ do
        let sub = makeSubscriber $ \v -> case v of
              Boolean True  -> [Boolean True]
              Boolean False -> [Boolean False]
              _             -> error "should not happen"
        ("#t #f", sub) ==> [Boolean True, Boolean False]
    ]
  where
    run :: (Monoid (m Value), Semigroup (m Value))
        => Text -> Subscriber m Value -> Either LangError (m Value)
    run src subs = fmap snd $ foldChainFromSrc "test" src
                 $ addSubscriber (removeSubscribers genesisChain) subs
    (val, subs) ==> y = run val subs @?= Right y

test_pretty :: [TestTree]
test_pretty =
    [ testProperty "pretty . parse == identity" $ \val ->
        let rendered = compactPretty val
            actual = parseTest rendered
            original = everywhere removeEnv val
            info = case actual of
              Left e -> "parse error in: " <> T.unpack rendered <> "\n"
                      <> e
              Right v -> "pretty: " <> T.unpack rendered <> "\n"
                      <> "reparsed: " <> show v <> "\n"
                      <> "original: " <> show original <> "\n"
        in counterexample info $ actual == Right original
    ]
  where
    everywhere :: Data a => (forall b. Data b => b -> b) -> a -> a
    everywhere f x = f (gmapT (everywhere f) x)
    -- | Environments are neither printed nor parsed, but are generated by the
    -- arbitrary instance.
    removeEnv :: forall b. Data b => (b -> b)
    removeEnv x = case (cast x, eqT :: Maybe (b :~: Value)) of
        (Just (Lambda arg body _), Just Refl) -> Lambda arg body Nothing
        _                                     -> x

test_env :: [TestTree]
test_env =
    [ testProperty "fromList . toList == identity" $ \(env :: Env) ->
        fromList (toList env) == env
    ]
