module Server.Common where

import           Protolude

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import           Radicle

data ReaderOrWriter = Reader | Writer

data Chain a = Chain
    { chainName      :: Text
    , chainState     :: Bindings (PrimFns Identity)
    , chainEvalPairs :: Seq.Seq (Value, Value)
    , chainLastIndex :: Maybe a
    , chainMode      :: ReaderOrWriter
    } deriving (Generic)

-- For efficiency we might want keys to also be mvars, but efficiency doesn't
-- matter for a test server.
newtype Chains a = Chains { getChains :: MVar (Map Text (Chain a)) }

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

advanceChain :: Chain a -> [Value] -> Either (LangError Value) ([Value], Bindings (PrimFns Identity))
advanceChain chain vals =
  let (r_, newSt) = runIdentity $ runLang (chainState chain) $ traverse eval vals
  in case r_ of
    Left e -> Left e
    Right r -> pure (r, newSt)
