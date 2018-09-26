module Radicle.Internal.CLI
    ( getConfig
    , getHistory
    )
where

import           Protolude
import           System.Directory (XdgDirectory(..), getXdgDirectory)

-- | Uses XDG_CONFIG_HOME if available.
getConfig :: IO FilePath
getConfig = getXdgDirectory XdgConfig "rad/config.rad"

-- | Uses XDG_CACHE_HOME if available.
getHistory :: IO FilePath
getHistory = getXdgDirectory XdgData "rad/config.rad"
