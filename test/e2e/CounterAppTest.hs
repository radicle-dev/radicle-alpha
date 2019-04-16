-- | Test the @example/radicle-counter@ app.
--
-- Requires access to an IPFS daemon.
module CounterAppTest
    ( test_counter_app
    ) where

import           Protolude

import           System.FilePath
import           Test.E2ESupport

test_counter_app :: TestTree
test_counter_app = testCaseSteps "counter app" $ \step -> using RadDaemon1 $ do
    machineId <- runTestCommand "rad-machines" ["create"]

    step "Initialize machine"
    void $ runCounter [machineId, "init"]

    initialValue <- runCounter [machineId, "get-value"]
    assertEqual "(get-value) on counter chain" "0" initialValue

    forM_ [(1::Int)..3] $ \i -> do
        step $ "Increment to " <> show i

        valueInc <- using RadDaemon2 $ runCounter [machineId, "increment"]
        assertEqual "(increment) on counter chain" (show i) valueInc

        valueGet <- runCounter [machineId, "get-value"]
        assertEqual "(get-value) on counter chain" (show i) valueGet
  where
    runCounter :: [Text] -> TestM Text
    runCounter args = do
        p <- asks projectDir
        let bin = p </> "examples" </> "radicle-counter"
        runTestCommand bin (map toS args)
