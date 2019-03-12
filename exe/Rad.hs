{-# LANGUAGE TypeApplications #-}
-- | Top-level radicle CLI. Calls sub-commands.
module Rad
    ( main
    ) where

import           Protolude

import           Data.List (lookup, stripPrefix)
import           Data.String (String)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           System.Directory
import           System.FilePath
import qualified System.Posix.Process as Posix

import           Paths_radicle (getBinDir, version)

-- | All command filenames are prefixed with this.
radPrefix :: String
radPrefix = "rad-"

runCommand :: [String] -> IO ()
runCommand [] = do
    cmds <- getSubCommands
    let availableCommands = sort $ map fst cmds <> builtinCommands
    putStr @Text "usage: rad <command> [<args>]\n\n"
    putStrLn @Text "Available commands:"
    putStr $ T.unlines $ ["   " <> T.pack c | c <- availableCommands]
runCommand ("version":_) =
    putStrLn radicleVersion
runCommand ("--version":_) =
    putStrLn radicleVersion
runCommand ("-v":_) =
    putStrLn radicleVersion
runCommand ("help":_) =
    runCommand []
runCommand ("--help":_) =
    runCommand []
runCommand ("-h":_) =
    runCommand []
runCommand (name:args) = do
    cmds <- getSubCommands
    case lookup name cmds of
        Just exe ->
            Posix.executeFile exe False args Nothing
        Nothing -> do
            die $ T.unwords
                [ "rad:", "'" <> toS name <> "'"
                , "is not a rad command. See 'rad help' for a list of available commands."
                ]

-- | Returns the radicle version
radicleVersion :: Text
radicleVersion = "radicle version " <> toS (showVersion version) :: Text

-- | Built-in commands.
builtinCommands :: [String]
builtinCommands =
    ["version", "help"]

-- | Returns list of Radicle sub-command binaries. These are all the
-- executables in 'getBinDir' starting with @rad@.
--
-- The first item is the command name, the second the absolute location
-- of the executable.
getSubCommands :: IO [(String, FilePath)]
getSubCommands = do
    searchDir <- getBinDir
    files <- listDirectory searchDir
    let commands = mapMaybe (stripPrefix radPrefix) files
    pure [(command, searchDir </> radPrefix <> command) | command <- commands]

main :: IO ()
main = runCommand =<< getArgs
