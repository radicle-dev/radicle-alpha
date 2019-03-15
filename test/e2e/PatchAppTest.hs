-- | Test the Radicle patch app.
module PatchAppTest
    ( test_patch_propose
    , test_patch_accept
    ) where

import           Protolude

import           Test.E2ESupport

initProjectWithCommit :: TestM Text
initProjectWithCommit = do
    prepareRadicle
    _ <- runTestCommand' "rad-project" ["init"] ["project-name", "project desc", "1"]
    _ <- runTestCommand "git" ["checkout", "-b", "f/test"]
    _ <- runTestCommand "touch" ["test"]
    _ <- runTestCommand "git" ["add", "test"]
    runTestCommand "git" ["commit", "--message", "first commit"]

test_patch_propose :: TestTree
test_patch_propose = testCaseSteps "patch propose" $ \step -> do
    initProjectWithCommit

    step "rad patch propose"
    commitSha <- runTestCommand "git" ["rev-parse", "--short", "HEAD"]
    _ <- runTestCommand "rad-patch" ["propose", "HEAD"]

    listOutput <- runTestCommand "rad-patch" ["list"]
    assertContains listOutput "state      #"
    assertContains listOutput "pending    0"

    showOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showOutput "pending 0"
    assertContains showOutput $ "From " <> commitSha
    assertContains showOutput "Comments\n---"

test_patch_accept :: TestTree
test_patch_accept = testCaseSteps "patch accept" $ \step -> do
    initProjectWithCommit

    step "rad patch propose"
    _ <- runTestCommand "rad-patch" ["propose", "HEAD"]

    step "rad patch accept"
    _ <- runTestCommand "git" ["checkout", "master"]
    _ <- runTestCommand "rad-patch" ["accept", "0"]

    showAcceptedOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showAcceptedOutput "accepted 0"

    logOutput <- runTestCommand "git" ["log", "master"]
    assertContains logOutput "first commit"
