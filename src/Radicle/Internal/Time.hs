module Radicle.Internal.Time where

import qualified Prelude
import           Protolude

import qualified Data.Time as Time

locale :: Time.TimeLocale
locale = Time.defaultTimeLocale

fmt :: Prelude.String
fmt = Time.iso8601DateFormat (Just "%H:%M:%SZ")

parseTime :: Text -> Maybe Time.UTCTime
parseTime = Time.parseTimeM False locale fmt . toS

formatTime :: Time.UTCTime -> Text
formatTime = toS . Time.formatTime locale fmt

unixSeconds :: Time.UTCTime -> Maybe Int
unixSeconds utc =
  let secs = Time.formatTime Time.defaultTimeLocale "%s" utc
  in case readEither secs of
       Left _           -> Nothing
       Right (i :: Int) -> pure $ fromIntegral i
