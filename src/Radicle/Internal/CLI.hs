module Radicle.Internal.CLI
    ( getRadicleFile
    , getHistoryFile
    )
where

import           Protolude
import           System.Directory
                 (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import           System.FilePath (takeDirectory)

-- | Location of a radicle related file.
-- Creates the parent directory if it does not exist.
--
-- See 'getXdgDirectory' 'XdgData' for how @~/.local/share@ is
-- determined.
getRadicleFile :: FilePath -> IO FilePath
getRadicleFile name = do
    file <- getXdgDirectory XdgData $ "radicle/" <> name
    createDirectoryIfMissing True (takeDirectory file)
    pure file

-- | Location of the radicle history, usually
-- @~/.local/share/radicle/history@.
getHistoryFile :: IO FilePath
getHistoryFile = getRadicleFile "history"
