module ProjectAppTest
    ( test_project_show_id
    , test_project_checkout
    , test_project_init_own_remote
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

test_project_init_own_remote :: TestTree
test_project_init_own_remote = testCase "project init own remote" $ do
    prepareRadicle
    _ <- runTestCommand' "rad-project" ["init"]
        [ "project-name"
        , "project desc"
        , "2"
        -- @.@ is a working remote URL that does not require an actual
        -- remote. We need a working remote becuase @project init@ does
        -- a @git fetch@.
        , "."]
    remoteUrl <- runTestCommand "git" ["remote", "get-url" ,"origin"]
    assertEqual "Remote URL does not match" "." remoteUrl
