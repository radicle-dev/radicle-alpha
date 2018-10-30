module Radicle.Internal.CLI
    ( getConfigFile
    , getHistoryFile
    )
where

import           Protolude
import           System.Directory (XdgDirectory(..), getXdgDirectory)

-- | Location of the radicle file to interpret.
-- Usually @~/.local/config/radicle/config.rad@.
--
-- See 'getXdgDirectory' XdgConfig' for how @~/.local/share@ is
-- determined.
getConfigFile :: IO FilePath
getConfigFile = getXdgDirectory XdgConfig "radicle/config.rad"

-- | Location of the radicle history. Usually
-- @~/.local/share/radicle/history@.
--
-- See 'getXdgDirectory' 'XdgData' for how @~/.local/share@ is
-- determined.
getHistoryFile :: IO FilePath
getHistoryFile = getXdgDirectory XdgData "radicle/history"
