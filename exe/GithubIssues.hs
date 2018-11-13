{-# LANGUAGE QuasiQuotes #-}

module GithubIssues (main) where

import           Protolude

import qualified Data.Map.Strict as Map
import           GitHub.Data.Issues
import qualified GitHub.Data.Options as GH
import qualified GitHub.Endpoints.Issues as GH

import           Radicle

main :: IO ()
main = do
  possibleIssues <- GH.issuesForRepo "oscoin" "radicle" mempty
  case possibleIssues of
       (Left error)   -> putStrLn $ "Error: " ++ show error
       (Right issues) -> do
         let v :: Value = toRad $ radIssue <$> toList issues
         putStr $ renderPrettyDef v

radIssue :: Issue -> Value
radIssue Issue{..} = toRad $ Map.fromList
    [ ( "body" :: Text, mt issueBody )
    , ( "title", String issueTitle )
    , ( "state", state issueState )
    ]
  where
    mt :: Maybe Text -> Value
    mt = String . fromMaybe ""
    state = \case GH.StateOpen -> [kword|open|]
                  GH.StateClosed -> [kword|closed|]
