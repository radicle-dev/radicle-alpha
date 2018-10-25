{-# LANGUAGE TemplateHaskell #-}
module Server where

import           API
import           Codec.Serialise
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           Control.Concurrent
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Protolude hiding (fromStrict)
import           Radicle
import           Servant
import           DB

-- * Types

newtype Exprs = Exprs { getExprs :: Seq Value }
    deriving (Generic, Eq, Ord)

data Chain = Chain
    { chainName  :: Text
    , chainState :: Bindings (PrimFns Identity)
    } deriving (Generic)

-- For efficiency we might want keys to also be mvars, but efficiency doesn't
-- matter for a test server.
newtype Chains = Chains { getChains :: MVar (Map Text Chain) }

-- * Helpers

insertExpr :: Connection -> Chains -> Text -> Value -> IO (Either Text ())
insertExpr conn chains name val = withMVar (getChains chains) $ \c -> do
    let x = Map.lookup name c
    let chain = fromMaybe (Chain name pureEnv) x
    let (r, s) = runIdentity $ runLang (chainState chain) (eval val)
    case r of
        Left e -> pure . Left $ "invalid expression: " <> show e
        Right o -> liftIO $ Right <$> insertExprDB conn name val

loadState :: Connection -> IO Chains
loadState conn = do
    createIfNotExists conn
    res <- getAllDB conn
    chainPairs <- forM res $ \(name, vals) -> do
        let st = runIdentity $ runLang _ $ foldM_ (\_ x -> eval x) () vals
        case st of
            (Left err, _) -> panic $ show err
            (Right _, st') -> do
                let c = Chain { chainName = name, chainState = st' }
                pure (name, c)
    chains' <- newMVar $ Map.fromList chainPairs
    pure $ Chains chains'

-- * Handlers

-- | Submit something to a chain. The value is expected to be a pair, with the
-- first item specifying the chain the value is being submitted to, and the
-- second the expression being submitted.
submit :: Connection -> Chains -> Value -> Handler ()
submit conn chains val = case val of
    List [String i, v] -> do
        res <- liftIO $ insertExpr conn chains i val
        case res of
            Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 err }
            Right v  -> pure ()
    _ -> throwError
       $ err400 { errBody = "Expecting pair with chain name and val" }

-- | Get all expressions submitted to a chain since 'index'.
since :: Connection -> Text -> Int -> Handler [Value]
since conn name index = liftIO $ getSinceDB conn name index

static :: Server Raw
static = serveDirectoryFileServer "static/"


-- * Main

main :: IO ()
main = do
    conn <- connectPostgreSQL "blah"
    st <- loadState conn
    let gzipSettings = def { gzipFiles = GzipPreCompressed GzipIgnore }
        app = simpleCors $ gzip gzipSettings (serve api (server conn st))
    args <- getArgs
    case args of
      [portStr] -> case readEither portStr of
          Right port -> run port app
          Left _     -> die "Expecting argument to be a port (integer)"
      [] -> run 80 app
      _ -> die "Expecting zero or one arguments (port)"


server :: Connection -> Chains -> Server API
server conn st = submit conn st :<|> since conn :<|> static
