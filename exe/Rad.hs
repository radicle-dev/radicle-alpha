{-# LANGUAGE TypeApplications #-}
-- | Top-level radicle CLI. Calls sub-commands.
module Rad
    ( main
    ) where

import           Protolude

import           Data.List (isPrefixOf)
import           Data.String (String)
import qualified Data.Text as T
import           System.Directory
import qualified System.Posix.Process as Posix

import           Paths_radicle (getBinDir)

-- | All command filenames are prefixed with this.
radPrefix :: String
radPrefix = "rad-"

runCommand :: [String] -> IO ()
runCommand [] = do
    bin <- getBinDir
    cmds <- availableCommands [bin]
    putStr @Text "usage: rad <command> [<args>]\n\n"
    putStrLn @Text "Available rad commands:"
    putStr $ T.unlines $ ["   " <> T.pack c | c <- cmds]
runCommand ("version":_) =
    putStrLn ("rad version 1.0" :: Text)
runCommand ("help":_) =
    runCommand []
runCommand ("--help":_) =
    runCommand []
runCommand ("-h":_) =
    runCommand []
runCommand (name:args) = do
    result <- findExecutable (radPrefix <> name)
    case result of
        Just exe ->
            Posix.executeFile exe False args Nothing
        Nothing -> do
            die $ T.unwords
                [ "rad:", "'" <> toS name <> "'"
                , "is not a rad command. See 'rad help' for a list of available commands."
                ]

-- | Built-in commands.
builtinCommands :: [String]
builtinCommands =
    ["version", "help"]

-- | Lists commands available for running.
availableCommands
    :: [FilePath]  -- ^ Directories to search
    -> IO [String] -- ^ Available commands
availableCommands dirs =
    concat <$> forM dirs (\dir -> do
        result <- try $ do
            xs <- listDirectory dir
            pure $ [drop (length radPrefix) x | x <- xs, toS radPrefix `isPrefixOf` x]
        pure $ case result of
            Left (_ :: IOException) -> builtinCommands
            Right xs                -> xs ++ builtinCommands)

main :: IO ()
main = runCommand =<< getArgs
