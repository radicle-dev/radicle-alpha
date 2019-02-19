-- | Functions for logging messages to @stdout@
--
-- All logging functions recive a message and a list of key-value pairs
-- with additional info. This will be rendered as follows.
--
-- @
--      logInfo "the message" [("foo", "5)]
--      -- prints "INFO   the message foo=5"
-- @
module Radicle.Daemon.Logging
    ( LogLevel(..)
    , MonadLog(..)
    , logDebug
    , logInfo
    , logError
    , logError'
    ) where

import           Protolude hiding (log)

data LogLevel = LogError | LogInfo | LogDebug
  deriving (Eq, Ord)

class (MonadIO m) => MonadLog m where
    askLogLevel :: m LogLevel


log :: MonadLog m => LogLevel -> Text -> [(Text, Text)] -> m ()
log targetLevel msg info = do
    currentLogLevel <- askLogLevel
    if currentLogLevel >= targetLevel
    then log' targetLevel msg info
    else pure ()

log' :: MonadIO m => LogLevel -> Text -> [(Text, Text)] -> m ()
log' targetLevel msg info = do
    putStrLn $ renderLogLevel targetLevel <> "   " <> msg <> renderInfo
  where
    renderLogLevel :: LogLevel -> Text
    renderLogLevel LogError = "ERROR"
    renderLogLevel LogInfo  = "INFO "
    renderLogLevel LogDebug = "DEBUG"

    renderInfo = foldMap (\(key, val) -> " " <> key <> "=" <> val) info


logDebug :: MonadLog m => Text -> [(Text,Text)] -> m ()
logDebug = log LogDebug

logInfo :: MonadLog m => Text -> [(Text,Text)] -> m ()
logInfo = log LogInfo

logError :: MonadLog m => Text -> [(Text,Text)] -> m ()
logError = log LogError

-- | Similar to 'logError' but does not require a current log level.
-- Always logs the error.
logError' :: MonadIO m => Text -> [(Text,Text)] -> m ()
logError' = log' LogError
