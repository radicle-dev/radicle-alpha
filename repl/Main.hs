module Main (main) where

import           Data.Semigroup ((<>))
import qualified Data.Text.IO as T
import           Options.Applicative
import           System.Environment

import           Radicle

main :: IO ()
main = do
    opts' <- execParser allOpts
    cfgSrc <- T.readFile =<< case configFile opts' of
        Nothing  -> getConfig
        Just cfg -> pure cfg
    hist <- case histFile opts' of
        Nothing -> getHistory
        Just h  -> pure h
    repl hist cfgSrc
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run the radicle REPL"
       <> header "rad - The radicle REPL"
        )

-- | Uses XDG_CONFIG_HOME if available.
getConfig :: IO FilePath
getConfig = do
    mCfgHome <- lookupEnv "XDG_CONFIG_HOME"
    pure $ case mCfgHome of
        Nothing      -> "$HOME/.config/rad/config.rad"
        Just cfgHome -> cfgHome <> "/rad/config.rad"

-- | Uses XDG_CACHE_HOME if available.
getHistory :: IO FilePath
getHistory = do
    mCfgHome <- lookupEnv "XDG_CACHE_HOME"
    pure $ case mCfgHome of
        Nothing      -> "$HOME/.cache/rad/history"
        Just cfgHome -> cfgHome <> "/rad/history"

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
