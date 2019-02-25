{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Main (main) where

import           Prelude

import           Data.Foldable (forM_)
import           Data.List (isPrefixOf)
import           System.Directory
import           System.FilePath ((</>))

import           Distribution.PackageDescription (PackageDescription)
import           Distribution.Simple
import           Distribution.Simple.InstallDirs (InstallDirs(..))
import           Distribution.Simple.LocalBuildInfo
                 (LocalBuildInfo(..), absoluteInstallDirs)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils (installExecutableFile)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {postCopy = copySubCommands}


-- | Installs all files matching @bin/rad-*@ into the configured
-- @bindir@, say @/usr/local/bin@.
copySubCommands :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copySubCommands _args copyFlags packageDescription localBuildInfo = do
    let copyDest' = fromFlag $ copyDest copyFlags
    let targetBindir = bindir $ absoluteInstallDirs packageDescription localBuildInfo copyDest'
    let verbosity = fromFlag $ configVerbosity $ configFlags localBuildInfo
    subCommands <- getSubCommands
    forM_ subCommands $ \name ->
        installExecutableFile verbosity (searchDir </> name) (targetBindir </> name)
  where
    radPrefix :: String
    radPrefix = "rad-"

    searchDir :: FilePath
    searchDir = "bin"

    getSubCommands :: IO [String]
    getSubCommands = do
        files <- listDirectory searchDir
        pure $ filter (isPrefixOf radPrefix) files
