-- | Test the @example/radicle-counter@ app.
--
-- Requires access to an IPFS daemon.
module CounterAppTest
    ( test_counter_app
    ) where

import           Prelude (String, unwords)
import           Protolude

import           Data.List (lookup)
import           System.Environment
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

test_counter_app :: TestTree
test_counter_app = testCaseSteps "counter app" $ \step -> do
        step "Create machine"
        machineId <- runTestCommand "rad-machines" ["create"]

        step "Initialize machine"
        void $ runTestCommand "examples/radicle-counter" [machineId, "init"]

        initialValue <- runTestCommand "examples/radicle-counter" [machineId, "get-value"]
        assertEqual "(get-value) on counter chain" "0" initialValue

        forM_ [(1::Int)..3] $ \i -> do
            step $ "Increment to " <> show i

            valueInc <- runTestCommand "examples/radicle-counter" [machineId, "increment"]
            assertEqual "(increment) on counter chain" (show i) valueInc

            valueGet <- runTestCommand "examples/radicle-counter" [machineId, "get-value"]
            assertEqual "(get-value) on counter chain" (show i) valueGet

-- | Run a command with the given arguments and return stdout.
--
-- If the command exists with a non-zero exit code an exception is
-- thrown.
--
-- Set the @RAD_IPFS_API_URL@ to a value that makes it work for local
-- tests with @test/docker-compose.yaml@.
--
-- Any trailing newlines are stripped from the output.
runTestCommand :: (HasCallStack) => FilePath -> [String] -> IO String
runTestCommand bin args = do
    env <- getEnvironment
    let procEnv = setDefault "RAD_IPFS_API_URL" "http://localhost:19301" env
    let procSpec = (proc bin args) { env = Just procEnv }
    (exitCode, out, err) <- readCreateProcessWithExitCode procSpec ""
    case exitCode of
        ExitSuccess -> pure $ trimNewline out
        ExitFailure _ ->
            let commandLine = bin <> " " <> unwords args
            in assertFailure $
                "Command failed: " <> commandLine <> "\n"
                <> "-- stdout ---------\n"
                <> out
                <> "-- stderr ---------\n"
                <> err
                <> "-------------------\n"
  where
    setDefault :: Eq key => key -> value -> [(key, value)] -> [(key, value)]
    setDefault defKey defValue items =
        case lookup defKey items of
            Nothing -> (defKey, defValue):items
            Just _  -> items

trimNewline :: String -> String
trimNewline = reverse . dropWhile (=='\n') . reverse
