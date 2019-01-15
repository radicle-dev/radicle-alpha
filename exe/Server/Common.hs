module Server.Common where

import Protolude

import qualified Data.Text as T

-- | Log a message to @stdout@.
--
-- The second argument is a list of key-value pairs that will be joined with "="
-- and appended to the message.
--
-- @
--      logInfo "the message" [("foo", "5)]
--      -- prints "INFO   the message foo=5"
-- @
logInfo :: MonadIO m => Text -> [(Text, Text)] -> m ()
logInfo msg dat = do
    putStrLn $ "INFO   " <> msg <> datString
  where
    datString = T.intercalate "" $ map (\(key, val) -> " " <> key <> "=" <> val) dat
