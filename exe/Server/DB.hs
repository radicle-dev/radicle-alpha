{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.DB
    ( insertExprDB
    , getSinceDB
    , prepareDatabase
    ) where

import           Protolude

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import           Radicle

-- | Insert a new value into the DB. Note that this should only be done after
-- validating the transaction, and atomically also updating the state.
insertExprDB :: Connection -> Text -> Value -> IO ()
insertExprDB conn name val
    = void $ execute conn sql (name, name, val)
  where
     sql = "INSERT INTO txs (id, chain, expr)"
        <> "VALUES ((SELECT (COALESCE(MAX(id),-1) + 1) FROM txs WHERE chain = ?), ?, ?)"

getSinceDB :: Connection -> Text -> Int -> IO [Value]
getSinceDB conn name fromIndex = do
    rows :: [(Int, Text)] <-
        query conn "SELECT id, expr FROM txs WHERE chain = ? AND id >= ? ORDER BY id ASC" (name, fromIndex)
    let valuesE = forM rows $ \(id, src) ->
            let srcName = "[db " <> name <> ":" <> show id <> "]"
            in parse srcName src
    case valuesE of
        Left err     -> panic (show err)
        Right values -> pure $ values

-- | Create the tables if they don't exist
prepareDatabase :: Connection -> IO ()
prepareDatabase conn = void $ execute_ conn sql
  where
    sql = "CREATE TABLE IF NOT EXISTS txs ("
       <> " id integer NOT NULL,"
       <> " chain text NOT NULL,"
       <> " expr text NOT NULL)"

instance ToField Value where
    toField = toField . renderCompactPretty
