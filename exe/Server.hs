-- | This file defines a server that can be used as a *centralized* remote for
-- chains.
module Server (main) where

import           Protolude hiding (fromStrict, option)

import           Data.ByteString.Lazy (fromStrict)
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
                 , Handler
                 , Raw
                 , ServantErr(..)
                 , Server
                 , err400
                 , serve
                 , serveDirectoryFileServer
                 )
import qualified STMContainers.Map as StmMap

import           Radicle
import           Radicle.Internal.MachineBackend.EvalServer
import           Server.Chains

-- * Types

newtype Exprs = Exprs { getExprs :: Seq Value }
    deriving (Generic, Eq, Ord)


type ServerApi
    = "chains" :> Capture "chain" Text :> (ChainSubmitEndpoint :<|> ChainSinceEndpoint)
 :<|> Raw

serverApi :: Proxy ServerApi
serverApi = Proxy

-- * Handlers

-- | Submit something to a chain. The value is expected to be a pair, with the
-- first item specifying the chain the value is being submitted to, and the
-- second the expressions being submitted. The server either accepts all or
-- rejects all expressions (though other systems may not provide this
-- guarantee).
submit :: Connection -> Chains -> Text -> Values -> Handler Value
submit conn chains name (Values vals) = do
    logInfo "Submitted expressions" [("machine", name), ("expression-count", show $ length vals)]
    res <- updateChain conn chains name vals
    case res of
        Left err -> throwError $ err400 { errBody = fromStrict $ encodeUtf8 err }
        Right id  -> pure $ Number $ toRational id


-- | Get all expressions submitted to a chain since 'index'.
since :: Connection -> Text -> Int -> Handler Values
since conn name index = do
    logInfo "Requested expressions" [("machine", name), ("from-index", show index)]
    liftIO $ Values <$> getChainLog conn name index

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
    prepareDatabase conn
    chainsMap <- StmMap.newIO
    let gzipSettings = def { gzipFiles = GzipPreCompressed GzipIgnore }
        app = simpleCors $ gzip gzipSettings (serve serverApi (server conn chainsMap))
    logInfo "Start listening" [("port", show $ serverPort opts')]
    run (serverPort opts') app
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run a centralized radicle server"
       <> header "radicle-server"
        )

server :: Connection -> Chains -> Server ServerApi
server conn chains = chainEndpoints :<|> static
  where
    chainEndpoints chainName = submit conn chains chainName :<|> since conn chainName
