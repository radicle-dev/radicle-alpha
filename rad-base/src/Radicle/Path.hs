
module Radicle.Path where

import           Paths_rad_base
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.FilePath (splitSearchPath, (</>))

-- | Importing works as follows:
--
--    (1) If the RADPATH environment variable exists, search there
--    (2) Otherwise search in data-dir
--    (3) Finally we search in the current directory.
--
-- RADPATH should be a colon-separated list of directories (just like PATH).
findModule :: FilePath -> IO (Maybe FilePath)
findModule file = do
    mrp <- lookupEnv "RADPATH"
    radPathDirs <- case mrp of
            Nothing -> (\x -> [x </> "rad"]) <$> getDataDir
            Just rp -> pure $ splitSearchPath rp
    let searchPath = radPathDirs ++ ["."]
    let doFind xs = case xs of
            [] -> pure Nothing
            x:xs' -> do
                exists <- doesFileExist (x </> file)
                if exists then pure (Just $ x </> file) else doFind xs'
    doFind searchPath
