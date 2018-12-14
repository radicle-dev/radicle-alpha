module Radicle.Internal.CLI where

import           Protolude
import           System.Directory
                 (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import           System.FilePath (takeDirectory)

-- | Location of a radicle related file, usually
-- @~/.local/share/radicle/file@.
--
-- Creates the parent directory if it does not exist.
--
-- See 'getXdgDirectory' 'XdgData' for how @~/.local/share@ is
-- determined.
radicleXdgFile :: Text -> IO FilePath
radicleXdgFile f = do
    file <- getXdgDirectory XdgData $ toS $ "radicle/" <> f
    createDirectoryIfMissing True (takeDirectory file)
    pure file

-- | Location of the radicle REPL history.
getHistoryFile :: IO FilePath
getHistoryFile = radicleXdgFile "history"

-- | Location of the local state file.
getLocalStateFile :: IO FilePath
getLocalStateFile = radicleXdgFile "state"
