module Server where

import           API
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Sequence as Seq
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Protolude hiding (fromStrict)
import           Radicle
import           Servant
import qualified STMContainers.Map as STMMap

-- * Main

main :: IO ()
main = do
    st <- newState
    let gzipSettings = def { gzipFiles = GzipPreCompressed GzipIgnore }
        app = simpleCors $ gzip gzipSettings (serve api (server st))
    args <- getArgs
    case args of
      [portStr] -> case readEither portStr of
          Right port -> run port app
          Left _     -> die "Expecting argument to be a port (integer)"
      [] -> run 80 app
      _ -> die "Expecting zero or one arguments (port)"


server :: Chains -> Server API
server st = submit st :<|> since st :<|> jsonOutputs st :<|> static

-- * Handlers

-- | Submit something to a chain. The value is expected to be a pair, with the
-- first item specifying the chain the value is being submitted to, and the
-- second the expression being submitted.
submit :: Chains -> Value -> Handler ()
submit st val = case val of
    List [String i, v] -> do
        r <- liftIO . atomically $ insertExpr st i v
        case r of
            Right () -> pure ()
            Left err -> throwError
                      $ err400 { errBody = fromStrict $ encodeUtf8 err }
    _ -> throwError
       $ err400 { errBody = "Expecting pair with chain name and val" }

getActivitySince :: Chains -> Text -> Int -> Handler [(Value, Value)]
getActivitySince st name index = do
    r <- liftIO . atomically $ getSince st name index
    case r of
        Nothing -> throwError $ err400 { errBody = "No such chain/index" }
        Just v  -> pure v

-- | Get all expressions submitted to a chain since 'index'.
since :: Chains -> Text -> Int -> Handler [Value]
since st name index =
    fmap fst <$> getActivitySince st name index

jsonOutputs :: Chains -> Text -> Handler [Maybe A.Value]
jsonOutputs st name = do
  vs <- fmap snd <$> getActivitySince st name 0
  pure (maybeJson <$> vs)

static :: Server Raw
static = serveDirectoryFileServer "static/"

-- * Helpers

insertExpr :: Chains -> Text -> Value -> STM (Either Text ())
insertExpr st name val = do
    x <- STMMap.lookup name $ getChains st
    let chain = fromMaybe (Chain name pureEnv mempty) x
    let (r, s) = runIdentity $ runLang (chainState chain) (eval val)
    case r of
        Left e -> pure . Left $ "invalid expression: " <> show e
        Right o -> Right <$> STMMap.insert
            (chain { chainState = s
                   , chainExprs = chainExprs chain Seq.|> (val, o) })
            name
            (getChains st)

getSince :: Chains -> Text -> Int -> STM (Maybe [(Value, Value)])
getSince st name index = do
    x <- STMMap.lookup name $ getChains st
    pure $ toList . Seq.drop index . chainExprs <$> x

-- * Types

data Chain = Chain
    { chainName  :: Text
    , chainState :: Bindings (PrimFns Identity)
    , chainExprs :: Seq (Value, Value)
    } deriving (Generic)

newtype Chains = Chains { getChains :: STMMap.Map Text Chain }

newState :: IO Chains
newState = Chains <$> STMMap.newIO
