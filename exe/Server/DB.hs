module DB where

import           Protolude
import           Database.PostgreSQL.Simple
import           Radicle

-- | Insert a new value into the DB. Note that this should only be done after
-- validating the transaction, and atomically also updating the state.
insertExprDB :: Connection -> Text -> Value -> IO ()
insertExprDB conn name val
    = void $ execute conn sql (name, name, val)
  where
     sql = "INSERT INTO txs VALUES (SELECT (SUCC(MAX(id)) WHERE name = ?) GROUP BY name, ?, ?)"

getSinceDB :: Connection -> Text -> Int -> IO [Value]
getSinceDB conn name index
    = query conn "SELECT expr FROM txs WHERE chain == ? AND id > ?" (name, index)

-- | Get all txs in all chains. Useful after a server restart.
getAll :: Connection -> IO [(Text, [Value])]
getAll conn
    = query_ conn "SELECT (chain, expr) FROM txs GROUP BY chain"

-- | Create the tables if they don't exist
createIfNotExists :: Connection -> IO ()
createIfNotExists conn = void $ execute_ conn sql
  where
    sql = "CREATE TABLE IF NOT EXISTS txs ("
       <> " id text NOT NULL,"
       <> " chain text NOT NULL,"
       <> " expr text NOT NULL)"
