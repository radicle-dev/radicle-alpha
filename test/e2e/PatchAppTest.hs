-- | Test the Radicle patch app.
module PatchAppTest
    ( test_patch_propose
    , test_patch_checkout
    , test_patch_comment
    , test_patch_retract
    , test_patch_accept
    , test_patch_unauthorized
    ) where

import           Protolude

import           Test.E2ESupport

initProjectWithPatch :: TestM Text
initProjectWithPatch = do
    prepareRadicle
    _ <- using RadDaemon1 $ runTestCommand' "rad-project" ["init"] ["project-name", "project desc", "1"]
    _ <- runTestCommand "git" ["checkout", "-b", "f/test"]
    _ <- runTestCommand "touch" ["test"]
    _ <- runTestCommand "git" ["add", "test"]
    _ <- runTestCommand "git" ["commit", "--message", "first commit"]
    using RadDaemon2 $ runTestCommand "rad-patch" ["propose", "HEAD"]

test_patch_propose :: TestTree
test_patch_propose = testCaseSteps "patch propose" $ \step -> do
    step "init project with patch"
    initProjectWithPatch

    commitSha <- runTestCommand "git" ["rev-parse", "--short", "HEAD"]

    listOutput <- using RadDaemon1 $ runTestCommand "rad-patch" ["list"]
    assertContains listOutput "state      #"
    assertContains listOutput "pending    0"

    showOutput <- using RadDaemon1 $ runTestCommand "rad-patch" ["show", "0"]
    assertContains showOutput "pending 0"
    assertContains showOutput $ "From " <> commitSha
    assertContains showOutput "Comments\n---"

test_patch_checkout :: TestTree
test_patch_checkout = testCaseSteps "patch checkout" $ \step -> do
    step "init project with patch"
    initProjectWithPatch

    step "rad patch checkout"
    _ <- runTestCommand "git" ["checkout", "master"]
    _ <- runTestCommand "rad-patch" ["checkout", "0"]

    branchOutput <- runTestCommand "git" ["branch"]
    assertContains branchOutput "* patch/0"

    logOutput <- runTestCommand "git" ["log"]
    assertContains logOutput "first commit"

    _ <- runTestCommand "git" ["checkout", "master"]
    logMasterOutput <- runTestCommand "git" ["log"]
    assertAbsence logMasterOutput "first commit"

test_patch_comment :: TestTree
test_patch_comment = testCaseSteps "patch comment" $ \step -> do
    step "init project with patch"
    initProjectWithPatch

    step "rad patch comment"
    _ <- runTestCommand "rad-patch" ["comment", "0", "Test commenting"]

    showOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showOutput "Test commenting"

test_patch_retract :: TestTree
test_patch_retract = testCaseSteps "patch retract" $ \step -> do
    step "init project with patch"
    initProjectWithPatch

    step "rad patch retract"
    _ <- runTestCommand "rad-patch" ["retract", "0"]

    showAcceptedOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showAcceptedOutput "retracted 0"

test_patch_accept :: TestTree
test_patch_accept = testCaseSteps "patch accept" $ \step -> do
    step "init project with patch"
    initProjectWithPatch

    step "rad patch accept"
    _ <- runTestCommand "git" ["checkout", "master"]
    _ <- runTestCommand "rad-patch" ["accept", "0"]

    showAcceptedOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showAcceptedOutput "accepted 0"

    logOutput <- runTestCommand "git" ["log", "master"]
    assertContains logOutput "first commit"

test_patch_unauthorized :: TestTree
test_patch_unauthorized = testCaseSteps "patch unauthorized actions" $ \step -> do
    step "init project with patch (maintainer)"
    initProjectWithPatch
    _ <- runTestCommand "git" ["checkout", "master"]

    step "rad patch reject fails (contributer)"
    _ <- runTestCommand "rad-key" ["create", "--force"]
    rejectOutput <- runTestCommandForError "rad-patch" ["reject", "0"]
    assertContains rejectOutput "Are you authorized to do so?"

    step "rad patch accept fails (contributer)"
    acceptOutput <- runTestCommandForError "rad-patch" ["accept", "0"]
    assertContains acceptOutput "Are you authorized to do so?"

    showOutput <- runTestCommand "rad-patch" ["show", "0"]
    assertContains showOutput "pending 0"
