{-# LANGUAGE TemplateHaskell #-}
module Server where

import           API
import           Codec.Serialise
import           Data.Acid
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map as Map
import           Data.SafeCopy
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Protolude hiding (fromStrict)
import           Radicle
import           Servant

-- * Types

newtype Exprs = Exprs { getExprs :: Seq Value }
    deriving (Generic, Eq, Ord)

$(deriveSafeCopy 0 'base ''Exprs)

data Chain = Chain
    { chainName  :: Text
    , chainState :: Bindings (PrimFns Identity)
    , chainExprs :: Exprs
    } deriving (Generic)

instance S.Serialize Value where
    put = S.putLazyByteString . serialise

instance SafeCopy Value where

newtype Chains = Chains { getChains :: Map Text Chain }

-- * Helpers

insertExpr :: Text -> Value -> Update Exprs (Either Text ())
insertExpr name val = do
    st <- get
    let x = Map.lookup name $ getChains st
    let chain = fromMaybe (Chain name pureEnv mempty) x
    let (r, s) = runIdentity $ runLang (chainState chain) (eval val)
    case r of
        Left e -> pure . Left $ "invalid expression: " <> show e
        Right o -> do
            put $ Chains $ Map.insert
                name
                (chain { chainState = s
                        , chainExprs = Exprs $ getExprs (chainExprs chain) Seq.|> (val, o) })
                (getChains st)
            pure $ Right ()

getSince :: Text -> Int -> Query Exprs (Maybe [(Value, Value)])
getSince name index = do
    x <- Map.lookup name <$> asks getChains
    pure $ toList . Seq.drop index . chainExprs <$> x

$(makeAcidic ''Exprs ['insertExpr, 'getSince])

createOrLoadState :: IO (AcidState Chains)
createOrLoadState = openLocalState $ Chains mempty

-- * Handlers

-- | Submit something to a chain. The value is expected to be a pair, with the
-- first item specifying the chain the value is being submitted to, and the
-- second the expression being submitted.
submit :: AcidState Chains -> Value -> Handler ()
submit st val = case val of
    List [String i, v] -> do
        r <- liftIO $ update st $ InsertExpr i v
        case r of
            Right () -> pure ()
            Left err -> throwError
                      $ err400 { errBody = fromStrict $ encodeUtf8 err }
    _ -> throwError
       $ err400 { errBody = "Expecting pair with chain name and val" }

getActivitySince :: AcidState Chains -> Text -> Int -> Handler [(Value, Value)]
getActivitySince st name index = do
    r <- liftIO $ query st $ GetSince name index
    case r of
        Nothing -> throwError $ err400 { errBody = "No such chain/index" }
        Just v  -> pure v

-- | Get all expressions submitted to a chain since 'index'.
since :: AcidState Chains -> Text -> Int -> Handler [Value]
since st name index =
    fmap fst <$> getActivitySince st name index

jsonOutputs :: AcidState Chains -> Text -> Handler [Maybe A.Value]
jsonOutputs st name = do
  vs <- fmap snd <$> getActivitySince st name 0
  pure (maybeJson <$> vs)

static :: Server Raw
static = serveDirectoryFileServer "static/"


-- * Main

main :: IO ()
main = do
    st <- createOrLoadState
    let gzipSettings = def { gzipFiles = GzipPreCompressed GzipIgnore }
        app = simpleCors $ gzip gzipSettings (serve api (server st))
    args <- getArgs
    case args of
      [portStr] -> case readEither portStr of
          Right port -> run port app
          Left _     -> die "Expecting argument to be a port (integer)"
      [] -> run 80 app
      _ -> die "Expecting zero or one arguments (port)"


server :: AcidState Chains -> Server API
server st = submit st :<|> since st :<|> jsonOutputs st :<|> static
