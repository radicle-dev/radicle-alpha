module ProjectAppTest
    ( test_project_show_id
    , test_project_checkout
    ) where

import           Protolude

import           Test.E2ESupport
import           UnliftIO.Directory

test_project_show_id :: TestTree
test_project_show_id = testCase "project show-id" $ do
    prepareRadicle

    _ <- runTestCommand' "rad-project" ["init"] ["project-name", "project desc", "1"]
    projectId <- runTestCommand "git" ["config", "--get" ,"radicle.project-id"]
    showIdOut <- runTestCommand' "rad-project" ["show-id"] []
    assertContains showIdOut projectId

test_project_checkout :: TestTree
test_project_checkout = testCase "project checkout" $ do
    prepareRadicle

    createDirectory "origin"
    projectId <- withCurrentDirectory "origin" $ do
        _ <- runTestCommand' "rad-project" ["init"] ["project-name", "project desc", "1"]
        runTestCommand "git" ["config", "--get" ,"radicle.project-id"]

    runTestCommand "rad-project" ["checkout", projectId]
    withCurrentDirectory "project-name" $ do
        showIdOut <- runTestCommand' "rad-project" ["show-id"] []
        assertContains showIdOut projectId
