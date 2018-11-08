-- | This file defines a server that can be used as a *centralized* remote for
-- chains.
module Server (main) where

import           Protolude hiding (fromStrict, option)

import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Database.PostgreSQL.Simple
                 (ConnectInfo(..), Connection, connect)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.Middleware.Gzip
                 (GzipFiles(..), def, gzip, gzipFiles)
import           Options.Applicative
                 ( Parser
                 , auto
                 , execParser
                 , fullDesc
                 , header
                 , help
                 , helper
                 , info
                 , long
                 , metavar
                 , option
                 , progDesc
                 , showDefault
                 , strOption
                 , value
                 )
import           Servant
                 ( (:<|>)(..)
                 , (:>)
                 , Capture
                 , Get
                 , Handler
                 , JSON
                 , Raw
                 , ServantErr(..)
                 , Server
                 , err400
                 , serve
                 , serveDirectoryFileServer
                 )

import           API
import           Radicle
import           Server.DB

-- * Types

newtype Exprs = Exprs { getExprs :: Seq Value }
    deriving (Generic, Eq, Ord)

data Chain = Chain
    { chainName      :: Text
    , chainState     :: Bindings (PrimFns Identity)
    , chainEvalPairs :: Seq.Seq (Value, Value)
    } deriving (Generic)

-- For efficiency we might want keys to also be mvars, but efficiency doesn't
-- matter for a test server.
newtype Chains = Chains { getChains :: MVar (Map Text Chain) }

type ServerApi
    = "chains" :> Capture "chain" Text :> (ChainSubmitEndpoint :<|> ChainSinceEndpoint)
 :<|> "outputs" :> Capture "chain" Text :> Get '[JSON] [Maybe A.Value]
 :<|> Raw

serverApi :: Proxy ServerApi
serverApi = Proxy


-- * Helpers

insertExpr :: Connection -> Chains -> Text -> [Value] -> IO (Either Text ())
insertExpr conn chains name vals = modifyMVar (getChains chains) $ \c -> do
    let x = Map.lookup name c
    let chain = fromMaybe (Chain name pureEnv mempty) x
    let (r :: Either (LangError Value) [Value], newSt) = runIdentity
                   $ runLang (chainState chain)
                   $ traverse eval vals
    case r of
        Left e  -> pure . (c ,) . Left $ "invalid expression: " <> show e
        Right valsRes -> do
             void $ traverse (insertExprDB conn name) vals
             let news = Seq.fromList $ zip vals valsRes
             let chain' = chain { chainState = newSt
                                , chainEvalPairs = chainEvalPairs chain Seq.>< news
                                }
             pure (Map.insert name chain' c, Right ())


-- | Load the state from the DB, returning an MVar with resulting state
-- and values.
loadState :: Connection -> IO Chains
loadState conn = do
    createIfNotExists conn
    res <- getAllDB conn
    chainPairs <- forM res $ \(name, vals) -> do
        let go acc x = eval x >>= \v -> pure (acc Seq.|> (x, v))
        let st = runIdentity $ runLang pureEnv $ foldM go mempty vals
        case st of
            (Left err, _) -> panic $ show err
            (Right pairs, st') -> do
                let c = Chain { chainName = name
                              , chainState = st'
                              , chainEvalPairs = pairs
                              }
                pure (name, c)
    chains' <- newMVar $ Map.fromList chainPairs
    pure $ Chains chains'


-- * Handlers

-- | Submit something to a chain. The value is expected to be a pair, with the
-- first item specifying the chain the value is being submitted to, and the
-- second the expression being submitted.
submit :: Connection -> Chains -> Text -> Values -> Handler Value
submit conn chains name (Values vals) = do
    res <- liftIO $ insertExpr conn chains name vals
    case res of
        Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 err }
        Right _  -> pure $ Keyword $ unsafeToIdent "ok"


-- | Get all expressions submitted to a chain since 'index'.
since :: Connection -> Text -> Int -> Handler Value
since conn name index = liftIO $ List <$> getSinceDB conn name index

jsonOutputs :: Chains -> Text -> Handler [Maybe A.Value]
jsonOutputs st name = do
    chains <- liftIO . readMVar $ getChains st
    pure $ case Map.lookup name chains of
        Nothing    -> []
        Just chain -> toList $ maybeJson . snd <$> chainEvalPairs chain

static :: Server Raw
static = serveDirectoryFileServer "static/"

-- * Opts

data Opts = Opts
    { connectionInfo :: ConnectInfo
    , serverPort     :: Int
    } deriving (Eq, Show, Read)

opts :: Parser Opts
opts = mkOpts
    <$> strOption
        ( long "host"
       <> help "postgres host"
       <> metavar "HOST"
       <> showDefault
       <> value "localhost"
        )
    <*> option auto
        ( long "pgport"
       <> help "postgres port"
       <> metavar "PGPORT"
       <> showDefault
       <> value 5432
        )
    <*> strOption
        ( long "user"
       <> help "postgres user"
       <> metavar "USER"
       <> showDefault
       <> value "postgres"
        )
    <*> strOption
        ( long "password"
       <> help "postgres password"
       <> metavar "PWD"
       <> showDefault
       <> value ""
        )
    <*> strOption
        ( long "db"
       <> help "postgres database name"
       <> metavar "DB"
       <> showDefault
       <> value "radserver"
        )
    <*> option auto
        ( long "port"
       <> help "server port"
       <> metavar "PORT"
       <> showDefault
       <> value 8000
        )
  where
    mkOpts h pgp u pw db p = Opts (ConnectInfo h pgp u pw db) p


-- * Main

main :: IO ()
main = do
    opts' <- execParser allOpts
    conn <- connect $ connectionInfo opts'
    st <- loadState conn
    let gzipSettings = def { gzipFiles = GzipPreCompressed GzipIgnore }
        app = simpleCors $ gzip gzipSettings (serve serverApi (server conn st))
    run (serverPort opts') app
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run a centralized radicle server"
       <> header "radicle-server"
        )

server :: Connection -> Chains -> Server ServerApi
server conn chains = chainEndpoints :<|> jsonOutputs chains :<|> static
  where
    chainEndpoints chainName = submit conn chains chainName :<|> since conn chainName
