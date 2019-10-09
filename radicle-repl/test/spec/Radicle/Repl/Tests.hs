{-# LANGUAGE QuasiQuotes #-}
module Radicle.Repl.Tests
    ( test_repl_primops ) where

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
import           Radicle.Internal.Arbitrary ()
import           Radicle.Internal.Core (asValue, noStack)
import           Radicle.Repl.TestCapabilities
import           Radicle.TH

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

-- Helper functions

int :: Integer -> Value
int = Number . fromIntegral
