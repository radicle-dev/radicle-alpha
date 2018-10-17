module Server.DB where

import           Database.PostgreSQL.Simple
import           Protolude

-- | Insert a new value into the DB. Note that this should only be done after
-- validating the transaction, and atomically also updating the state.
insertExprDB :: Connection -> Text -> Value -> IO ()
insertExprDB conn name val
    = void . execute conn $ "INSERT INTO txs VALUES (?, ?)" (name, val)

getSinceDB :: Connection -> Text -> Int -> IO [Value]
getSinceDB conn name index
    = void . query $ "SELECT exprs FROM txs WHERE chain == ? AND id > ?" (name, index)
