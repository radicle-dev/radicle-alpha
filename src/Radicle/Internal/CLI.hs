module Radicle.Internal.CLI where

import           Protolude
import           System.Environment (lookupEnv)

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
