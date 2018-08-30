module Server where

import           API
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Sequence as Seq
import           Network.Wai.Handler.Warp
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
        app = gzip gzipSettings (serve api (server st))
    args <- getArgs
    case args of
      [portStr] -> case readEither portStr of
          Right port -> run port app
          Left _ -> die "Expecting argument to be a port (integer)"
      [] -> run 80 app
      _ -> die "Expecting zero or one arguments (port)"


server :: Chains -> Server API
server st = submit st :<|> since st :<|> static

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

-- | Get all expressions submitted to a chain since 'index'.
since :: Chains -> Text -> Int -> Handler [Value]
since st name index = do
    r <- liftIO . atomically $ getSince st name index
    case r of
        Nothing -> throwError $ err400 { errBody = "No such chain/index" }
        Just v  -> pure v

static :: Server Raw
static = serveDirectoryFileServer "static/"
-- * Helpers

insertExpr :: Chains -> Text -> Value -> STM (Either Text ())
insertExpr st name val = do
    x <- STMMap.lookup name $ getChains st
    let chain = fromMaybe (Chain name pureEnv mempty) x
    let (r, s) = runIdentity $ runLang (chainState chain) (eval val)
    case r of
        Left _ -> pure $ Left "invalid expression"
        Right _ -> Right <$> STMMap.insert
            (chain { chainState = s
                   , chainExprs = chainExprs chain Seq.|> val })
            name
            (getChains st)

getSince :: Chains -> Text -> Int -> STM (Maybe [Value])
getSince st name index = do
    x <- STMMap.lookup name $ getChains st
    pure $ toList . Seq.drop index . chainExprs <$> x

-- * Types

data Chain = Chain
    { chainName  :: Text
    , chainState :: Bindings Identity
    , chainExprs :: Seq Value
    } deriving (Generic)

newtype Chains = Chains { getChains :: STMMap.Map Text Chain }

newState :: IO Chains
newState = Chains <$> STMMap.newIO
