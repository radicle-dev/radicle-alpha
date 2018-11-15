{-# LANGUAGE QuasiQuotes #-}

module GithubIssues (main) where

import           Protolude hiding (state)

import qualified Data.Map.Strict as Map
import           Data.Time
import           GitHub.Data.Issues
import           GitHub.Data.Name (Name(..))
import qualified GitHub.Data.Options as GH
import qualified GitHub.Endpoints.Issues as GH
import qualified GitHub.Endpoints.Issues.Comments as GH
import           Options.Applicative

import           Radicle

main :: IO ()
main = do
  Opts{..} <- execParser allOpts
  let auth = GH.OAuth . toS <$> token
  is_ <- runExceptT (getIssues auth owner repo)
  case is_ of
    Right is -> do
      let t = renderPrettyDef (toRad (uncurry radIssue <$> is) :: Value)
      case output of
        Just file -> writeFile file t
        Nothing   -> putStrLn t
    Left e -> print e
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Exports all issues from a GitHub repo as radicle data."
       <> header "Radicle GitHub issues exporter"
        )

getIssues :: Maybe GH.Auth -> Text -> Text -> ExceptT GH.Error IO [(Issue, [IssueComment])]
getIssues auth owner repo = do
  is <- ExceptT $ GH.issuesForRepo' auth (N owner) (N repo) GH.stateAll
  let issues = filter notPR (toList is)
  css <- traverse (getComments auth owner repo) issues
  pure $ zip issues css
  where
    notPR Issue{ issuePullRequest = Nothing} = True
    notPR _                                  = False

getComments :: Maybe GH.Auth -> Text -> Text -> Issue -> ExceptT GH.Error IO [IssueComment]
getComments auth owner repo Issue{..} = do
  -- You would think we should query by issue ID, given the types, but nonono!
  -- That would be too easy!
  -- (This will probably be fixed upstream, soon, by me.)
  cs <- ExceptT $ GH.comments' auth (N owner) (N repo) (GH.mkId (Proxy :: Proxy Issue) issueNumber)
  pure (toList cs)

radIssue :: Issue -> [IssueComment] -> Value
radIssue Issue{..} cs = dict $
    [ ( [kword|github-username|], author issueUser )
    , ( [kword|title|], String issueTitle )
    , ( [kword|body|], mt issueBody )
    , ( [kword|github-assignee-usernames|], toRad $ author <$> toList issueAssignees)
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
  , ( [kword|github-username|], author issueCommentUser )
  , ( [kword|created-at|], date issueCommentCreatedAt )
  ]

dict :: [(Value, Value)] -> Value
dict = Dict . Map.fromList

date :: UTCTime -> Value
date = String . toS . formatTime defaultTimeLocale fmt
  where
    fmt = iso8601DateFormat (Just "%H:%M:%SZ")

author :: GH.SimpleUser -> Value
author = String . GH.untagName . GH.simpleUserLogin

-- CLI

data Opts = Opts
    { owner  :: Text
    , repo   :: Text
    , output :: Maybe FilePath
    , token  :: Maybe Text
    }

opts :: Parser Opts
opts = Opts
    <$> strArgument
        ( metavar "OWNER"
        <> help "GitHub owner username."
        )
    <*> strArgument
        ( metavar "REPO"
        <> help "GitHub repo name."
        )
    <*> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Output file with the GitHub issues as radicle data."
        ))
    <*> optional (strOption
        ( long "token"
       <> short 't'
       <> metavar "TOKEN"
       <> help
           ( "GitHub personal access token. "
          <> "If the repo has a large amount of issues, this will "
          <> "be needed because of rate-limiting. "
          <> "Go to https://github.com/settings/tokens to generate a token."
           )
        ))
