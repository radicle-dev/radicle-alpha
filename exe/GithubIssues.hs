{-# LANGUAGE QuasiQuotes #-}

module GithubIssues (main) where

import           Protolude hiding (state)

import           Control.Error
import qualified Data.Map.Strict as Map
import           Data.Time
import           GitHub.Data.Issues
import           GitHub.Data.Name (Name(..))
import qualified GitHub.Data.Options as GH
import qualified GitHub.Endpoints.Issues as GH
import qualified GitHub.Endpoints.Issues.Comments as GH

import           Radicle

main :: IO ()
main = do
  is_ <- runExceptT (getIssues "oscoin" "radicle")
  case is_ of
    Right is -> putStrLn (renderPrettyDef (toRad (uncurry radIssue <$> is) :: Value))
    Left e -> print e

getIssues :: Text -> Text -> ExceptT GH.Error IO [(Issue, [IssueComment])]
getIssues owner repo = do
  is <- ExceptT $ GH.issuesForRepo (N owner) (N repo) GH.stateAll
  let issues = filter notPR (toList is)
  css <- traverse (getComments owner repo) issues
  pure $ zip issues css
  where
    notPR Issue{ issuePullRequest = Nothing} = True
    notPR _                                  = False

getComments :: Text -> Text -> Issue -> ExceptT GH.Error IO [IssueComment]
getComments owner repo Issue{..} = do
  cs <- ExceptT $ GH.comments (N owner) (N repo) issueId
  pure (toList cs)

radIssue :: Issue -> [IssueComment] -> Value
radIssue Issue{..} cs = dict $
    [ ( [kword|github-author|], author issueUser )
    , ( [kword|title|], String issueTitle )
    , ( [kword|body|], mt issueBody )
    , ( [kword|github-assignees|], toRad $ author <$> toList issueAssignees)
    , ( [kword|state|], state issueState )
    , ( [kword|labels|]
      , toRad $ GH.untagName . GH.labelName <$> toList issueLabels
      )
    , ( [kword|created-at|], date issueCreatedAt )
    , ( [kword|comments|], toRad (radComment <$> cs) )
    ] <> catMaybes
    [ ( [kword|closed-at|],) . date <$> issueClosedAt ]
  where
    mt = String . fromMaybe ""
    state = \case GH.StateOpen -> [kword|open|]
                  GH.StateClosed -> [kword|closed|]

radComment :: IssueComment -> Value
radComment IssueComment{..} = dict
  [ ( [kword|body|], String issueCommentBody )
  , ( [kword|github-author|], author issueCommentUser )
  , ( [kword|created-at|], date issueCommentCreatedAt )
  ]

dict = Dict . Map.fromList

date :: UTCTime -> Value
date = String . toS . formatTime defaultTimeLocale fmt
  where
    fmt = iso8601DateFormat (Just "%H:%M:%S")

author :: GH.SimpleUser -> Value
author = String . GH.untagName . GH.simpleUserLogin
