module Radicle.Internal.CLI
    ( getConfigFile
    , getHistoryFile
    )
where

import           Protolude
import           System.Directory
                 (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import           System.FilePath (takeDirectory)

-- | Location of the radicle file to interpret.
-- Usually @~/.local/config/radicle/config.rad@.
--
-- See 'getXdgDirectory' XdgConfig' for how @~/.local/share@ is
-- determined.
getConfigFile :: IO FilePath
getConfigFile = getXdgDirectory XdgConfig "radicle/config.rad"

-- | Location of the radicle history, usually
-- @~/.local/share/radicle/history@.
--
-- Creates the parent directory if it does not exist.
--
-- See 'getXdgDirectory' 'XdgData' for how @~/.local/share@ is
-- determined.
getHistoryFile :: IO FilePath
getHistoryFile = do
    file <- getXdgDirectory XdgData "radicle/history"
    createDirectoryIfMissing True (takeDirectory file)
    pure file
