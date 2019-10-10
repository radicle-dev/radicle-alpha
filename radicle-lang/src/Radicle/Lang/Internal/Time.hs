module Radicle.Lang.Internal.Time where

import qualified Prelude
import           Protolude

import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time

locale :: Time.TimeLocale
locale = Time.defaultTimeLocale

fmt :: Prelude.String
fmt = Time.iso8601DateFormat (Just "%H:%M:%SZ")

parseTime :: Text -> Maybe Time.UTCTime
parseTime = Time.parseTimeM False locale fmt . toS

formatTime :: Time.UTCTime -> Text
formatTime = toS . Time.formatTime locale fmt

unixSeconds :: Time.UTCTime -> Int64
unixSeconds utc = Time.systemSeconds $ Time.utcToSystemTime utc

unixSecondsToUtc :: Int64 -> Time.UTCTime
unixSecondsToUtc seconds = Time.systemToUTCTime $ Time.MkSystemTime seconds 0
