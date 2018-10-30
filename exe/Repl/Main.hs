module Main (main) where

import           Options.Applicative
import           Protolude

import           Radicle

main :: IO ()
main = do
    opts' <- execParser allOpts
    cfgFile <- case configFile opts' of
        Nothing  -> getConfigFile
        Just cfg -> pure cfg
    cfgSrc <- readFile cfgFile
    hist <- case histFile opts' of
        Nothing -> getHistoryFile
        Just h  -> pure h
    repl (Just hist) (toS cfgFile) cfgSrc replBindings
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run the radicle REPL"
       <> header "rad - The radicle REPL"
        )

data Opts = Opts
    { configFile :: Maybe FilePath
    , histFile   :: Maybe FilePath
    }

opts :: Parser Opts
opts = Opts
    <$> optional (strOption
        ( long "config"
       <> metavar "FILE"
       <> help "rad configuration file"
        ))
    <*> optional (strOption
        ( long "histfile"
       <> metavar "FILE"
       <> help "repl history file"
        ))
