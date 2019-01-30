{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Distribution.PackageDescription (PackageDescription)
import           Distribution.Simple
import           Distribution.Simple.InstallDirs (CopyDest(..), InstallDirs(..))
import           Distribution.Simple.LocalBuildInfo
                 (LocalBuildInfo(..), absoluteInstallDirs)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils (copyFileTo, installExecutableFile)
import           Distribution.Verbosity (deafening)
import           System.Directory (getCurrentDirectory)
import           System.Environment (getEnv)
import           System.Exit (ExitCode, exitSuccess)
import           System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    {postCopy = copyRadFiles, postBuild = buildRadFiles}

buildRadFiles :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildRadFiles _ b@BuildFlags{buildVerbosity} pkg lbi = do
    cd <- getCurrentDirectory

    let src  = cd </> "bin" </> "rad-ipfs"
    let dst  = bindir (absoluteInstallDirs pkg lbi NoCopyDest) </> "rad-ipfs"
    let verb = fromFlag $ configVerbosity $ configFlags lbi

    installExecutableFile verb src dst

copyRadFiles :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyRadFiles _ _ pkg lbi = do
    cd   <- getCurrentDirectory
    home <- getEnv "HOME"

    let src  = bindir (absoluteInstallDirs pkg lbi NoCopyDest) </> "rad-ipfs"
    let verb = fromFlag $ configVerbosity $ configFlags lbi
    let dest = home </> ".local/bin" </> "rad-ipfs"

    installExecutableFile verb src dest
