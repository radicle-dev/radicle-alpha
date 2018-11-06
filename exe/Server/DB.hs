{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.DB where

import           Data.List (groupBy, head)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField(..))
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField (ToField(..))
import           Protolude hiding (head)
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
getSinceDB conn name index
    = query conn "SELECT expr FROM txs WHERE chain = ? AND id >= ? ORDER BY id ASC" (name, index)

-- | Get all txs in all chains. Useful after a server restart.
getAllDB :: Connection -> IO [(Text, [Value])]
getAllDB conn = do
    res :: [(Int, Text, Value)] <- query_ conn "SELECT id, chain, expr FROM txs"
    let  grouped :: [[(Text, Value)]]
         grouped
            = (\(_, a, b) -> (a, b)) . fmap (sortOn (\(a,_,_) -> a))
            <$> groupBy (\(_, l, _) (_, r, _) -> l == r) res
    pure [ (fst (head each), snd <$> each) | each <- grouped ]


-- | Create the tables if they don't exist
createIfNotExists :: Connection -> IO ()
createIfNotExists conn = void $ execute_ conn sql
  where
    sql = "CREATE TABLE IF NOT EXISTS txs ("
       <> " id integer NOT NULL,"
       <> " chain text NOT NULL,"
       <> " expr text NOT NULL)"

instance ToField Value where
    toField = toField . renderCompactPretty

instance FromField Value where
    fromField f bs = fromField f bs >>= \x -> case parse "DB" x of
        Left _  -> mzero
        Right v -> pure v

instance FromRow Value where
    fromRow = field
