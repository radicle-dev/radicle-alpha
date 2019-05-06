-- | Test that the Daemon properly serves HTML defined in the machine.
module HtmlTest (test_html) where

import           Protolude

import           Radicle.Daemon.Client
import           Radicle.Daemon.HttpApi (HtmlText(..))
import           Test.E2ESupport

machineHtml :: Text
machineHtml = "<html><body><p>hi</p></body></html>"

code :: Text
code =
    "(import prelude/machine '[send! new-machine!] :unqualified)" <>
    "(def id (new-machine!))" <>
    "(send! id (def get-html '(fn [] \"" <> machineHtml <> "\")))" <>
    "(print! id)"

initializeMachine :: TestM Text
initializeMachine = using RadDaemon1 $ runTestCommand' "rad-repl" [] [code]

test_html :: TestTree
test_html = testCaseSteps "machine html" $ \step -> do
    step "initialize"
    machine <- initializeMachine
    cli <- liftIO newClient
    t <- liftIO . runExceptT $ getMachineIndex cli (MachineId machine)
    assertEqual "Machine servers right html" t (Right $ HtmlText machineHtml)
