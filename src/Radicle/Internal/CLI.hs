module Radicle.Internal.CLI 
    ( getConfig
    , getHistory
    )
where

import           Protolude
import           Prelude (fail)
import           System.Environment (lookupEnv)

-- | Uses XDG_CONFIG_HOME if available.
getConfig :: IO FilePath
getConfig = do
    let configPaths = [ lookupEnv "XDG_CONFIG_HOME"
                      , (fmap.fmap) (<> "/.config") (lookupEnv "HOME")
                      ]
    mCfgHome <- firstMaybeM configPaths
    case mCfgHome of
        Nothing      -> fail "Can't find config path: Neither $XDG_CONFIG_HOME nor $HOME are set"
        Just cfgHome -> pure $ cfgHome <> "/rad/config.rad"

-- | Uses XDG_CACHE_HOME if available.
getHistory :: IO FilePath
getHistory = do
    let cachePaths = [ lookupEnv "XDG_CACHE_HOME"
                     , (fmap.fmap) (<> "/.cache") (lookupEnv "HOME")
                     ]
    mCacheHome <- firstMaybeM cachePaths
    case mCacheHome of
        Nothing        -> fail "Can't find config path: Neither $XDG_CACHE_HOME nor $HOME are set"
        Just cacheHome -> pure $ cacheHome <> "/rad/config.rad"


firstMaybeM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstMaybeM = foldM (\m x -> maybe x (pure . Just) m) Nothing
