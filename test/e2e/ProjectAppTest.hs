module ProjectAppTest
    ( test_project_show_id
    ) where

import           Protolude

import           Test.E2ESupport

test_project_show_id :: TestTree
test_project_show_id = testCase "project show-id" $ do
    _ <- runTestCommand "rad-key" ["create"]
    _ <- runTestCommand "git" ["config", "--global", "user.name", "Alice"]
    _ <- runTestCommand "git" ["config", "--global", "user.email", "alice@example.com"]
    _ <- runTestCommand' "rad-project" ["init"] ["project-name", "project desc", "1"]
    projectId <- runTestCommand "git" ["config", "--get" ,"radicle.project-id"]
    showIdOut <- runTestCommand' "rad-project" ["show-id"] []
    assertContains showIdOut projectId
