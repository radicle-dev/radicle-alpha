-- | Test the Radicle patch app.
module PatchAppTest
    ( test_patch_propose
    ) where

import           Protolude

import           Test.E2ESupport

test_patch_propose :: TestTree
test_patch_propose = testCaseSteps "patch propose" $ \step -> do
    step "Init project"
    _ <- runTestCommand "rad-key" ["create"]
    _ <- runTestCommand "git" ["config", "--global", "user.name", "Alice"]
    _ <- runTestCommand "git" ["config", "--global", "user.email", "alice@example.com"]
    _ <- runTestCommand' "rad-project" ["init"] ["project-name", "project desc", "1"]
    _ <- runTestCommand "git" ["checkout", "-b", "f/test"]
    _ <- runTestCommand "touch" ["test"]
    _ <- runTestCommand "git" ["add", "test"]
    _ <- runTestCommand "git" ["commit", "--message", "first commit"]
    commitSha <- runTestCommand "git" ["rev-parse", "--short", "HEAD"]

    step "rad patch propose"
    _ <- runTestCommand "rad-patch" ["propose", "HEAD"]

    listOutput <- runTestCommand "rad-patch" ["list"]
    assertContains listOutput "state      #"
    assertContains listOutput "pending    0"

    showOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showOutput "pending 0"
    assertContains showOutput $ "From " <> commitSha
    assertContains showOutput "Comments\n---"

    step "rad patch accept"
    _ <- runTestCommand "git" ["checkout", "master"]
    _ <- runTestCommand "rad-patch" ["accept", "0"]

    showAcceptedOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showAcceptedOutput "accepted 0"

    logOutput <- runTestCommand "git" ["log", "master"]
    assertContains logOutput "first commit"
