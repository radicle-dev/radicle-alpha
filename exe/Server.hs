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

import           Radicle
import           Radicle.Internal.MachineBackend.EvalServer
import           Server.Common
import           Server.DB

-- * Types

newtype Exprs = Exprs { getExprs :: Seq Value }
    deriving (Generic, Eq, Ord)

type ServerApi
    = "chains" :> Capture "chain" Text :> (ChainSubmitEndpoint :<|> ChainSinceEndpoint)
 :<|> "outputs" :> Capture "chain" Text :> Get '[JSON] [Maybe A.Value]
 :<|> Raw

serverApi :: Proxy ServerApi
serverApi = Proxy

type HttpChains = Chains Text Int ()

-- * Helpers

-- | Returns the ID of the last inserted expression which is the chain
-- length minus one.
insertExpr :: Connection -> HttpChains -> Text -> [Value] -> IO (Either Text Int)
insertExpr conn chains name vals = modifyMVar (getChains chains) $ \c -> do
    let maybeChain = Map.lookup name c
    chain <- case maybeChain of
        Nothing -> do
            logInfo "Creating new machine" [("machine", name)]
            pure $ Chain name pureEnv mempty Nothing Writer ()
        Just chain' -> pure chain'
    case advanceChain chain vals of
        Left e  -> do
            logInfo "Submitted expressions failed to evaluate" [("machine", name)]
            pure . (c ,) . Left $ renderPrettyDef e
        Right (valsRes, newSt) -> do
             traverse_ (insertExprDB conn name) vals
             let news = Seq.fromList $ zip vals valsRes
             let pairs = chainEvalPairs chain Seq.>< news
             let lastIndex = Seq.length pairs - 1
             let chain' = chain { chainState = newSt
                                , chainEvalPairs = pairs
                                , chainLastIndex = Just lastIndex
                                }
             pure (Map.insert name chain' c, Right lastIndex )


-- | Load the state from the DB, returning an MVar with resulting state
-- and values.
loadState :: Connection -> IO HttpChains
loadState conn = do
    createIfNotExists conn
    res <- getAllDB conn
    chainPairs <- forM res $ \(name, vals) -> do
        let go acc x = eval x >>= \v -> pure (acc Seq.|> (x, v))
        let st = runIdentity $ runLang pureEnv $ foldM go mempty vals
        case st of
            (Left err, _) -> do
                logInfo "Failed to load machine" [("machine", name)]
                panic $ show err
            (Right pairs, st') -> do
                let c = Chain { chainName = name
                              , chainState = st'
                              , chainEvalPairs = pairs
                              , chainLastIndex = Just $ Seq.length pairs - 1
                              , chainMode = Writer
                              }
                pure (name, c)
    chains' <- newMVar $ Map.fromList chainPairs
    logInfo "Loaded machines into memory" [("machine-count", show $ length chainPairs)]
    pure $ Chains chains'


-- * Handlers

-- | Submit something to a chain. The value is expected to be a pair, with the
-- first item specifying the chain the value is being submitted to, and the
-- second the expressions being submitted. The server either accepts all or
-- rejects all expressions (though other systems may not provide this
-- guarantee).
submit :: Connection -> HttpChains -> Text -> Values -> Handler Value
submit conn chains name (Values vals) = do
    logInfo "Submitted expressions" [("machine", name), ("expression-count", show $ length vals)]
    res <- liftIO $ insertExpr conn chains name vals
    case res of
        Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 err }
        Right id  -> pure $ Number $ toRational id


-- | Get all expressions submitted to a chain since 'index'.
since :: Connection -> Text -> Int -> Handler Values
since conn name index = do
    logInfo "Requested expressions" [("machine", name), ("from-index", show index)]
    liftIO $ Values <$> getSinceDB conn name index

jsonOutputs :: HttpChains -> Text -> Handler [Maybe A.Value]
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
    logInfo "Start listening" [("port", show $ serverPort opts')]
    run (serverPort opts') app
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run a centralized radicle server"
       <> header "radicle-server"
        )

server :: Connection -> HttpChains -> Server ServerApi
server conn chains = chainEndpoints :<|> jsonOutputs chains :<|> static
  where
    chainEndpoints chainName = submit conn chains chainName :<|> since conn chainName
