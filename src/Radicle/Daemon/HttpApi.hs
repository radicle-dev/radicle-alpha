{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Daemon.HttpApi
    ( QueryRequest(..)
    , QueryResponse(..)
    , SendRequest(..)
    , SendResponse(..)
    , NewResponse(..)
    , DaemonApi
    , daemonApi
    , swagger
    ) where

import           Protolude

import qualified Data.Aeson as A
import           Data.Swagger
import           Data.Version (showVersion)
import           Radicle.Daemon.Ipfs (MachineId(..))
import           Servant.API
import           Servant.Swagger

import Radicle
import qualified Paths_radicle as Radicle

instance FromHttpApiData MachineId where
  parseUrlPiece = Right . MachineId . toS
  parseQueryParam = parseUrlPiece

newtype QueryRequest = QueryRequest { expression :: Value }
  deriving (Generic)

instance A.ToJSON QueryRequest
instance A.FromJSON QueryRequest

newtype QueryResponse = QueryResponse { result :: Value }
  deriving (Generic)

instance A.ToJSON QueryResponse
instance A.FromJSON QueryResponse

newtype SendRequest = SendRequest { expressions :: [Value] }
  deriving (Generic)

instance A.FromJSON SendRequest

newtype SendResponse = SendResponse
  { results :: [Value]
  } deriving (Generic)

instance A.ToJSON SendResponse

newtype NewResponse = NewResponse
  { machineId :: MachineId
  } deriving (Generic)

instance A.ToJSON NewResponse

-- * APIs

type Query = "query" :> ReqBody '[JSON] QueryRequest :> Post '[JSON] QueryResponse
type Send  = "send"  :> ReqBody '[JSON] SendRequest :> Post '[JSON] SendResponse
type New   = "new"   :> Post '[JSON] NewResponse

type Machines = "machines" :> Capture "machineId" MachineId :> ( Query :<|> Send ) :<|> New
type Docs = "docs" :> Get '[JSON] Swagger

type DaemonApi = "v0" :> ( Machines :<|> Docs )

daemonApi :: Proxy DaemonApi
daemonApi = Proxy

-- * Docs

instance ToParamSchema MachineId where
  toParamSchema _ = mempty { _paramSchemaType = SwaggerString }
instance ToSchema MachineId where
  declareNamedSchema pxy = pure $
    NamedSchema (Just "MachineId") $
      mempty { _schemaDescription = Just "The ID (i.e. IPNS name) of an IPFS machine."
             , _schemaParamSchema = toParamSchema pxy }

instance ToSchema Value where
  declareNamedSchema _ = pure $
    NamedSchema (Just "RadicleValue") $
      mempty { _schemaDescription = Just "A radicle value formatted according to radicle's S-expression syntax."
             , _schemaParamSchema = mempty {_paramSchemaType = SwaggerString } }

instance ToSchema QueryRequest
instance ToSchema QueryResponse
instance ToSchema SendRequest
instance ToSchema SendResponse
instance ToSchema NewResponse

instance ToSchema Swagger where
  declareNamedSchema _ = pure $
    NamedSchema (Just "Swagger") $
      mempty { _schemaDescription = Just "This swagger spec."
             , _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }}

swagger :: Swagger
swagger = swag { _swaggerInfo = inf }
  where
    inf = mempty { _infoTitle = "Radicle daemon"
                 , _infoDescription = Just desc
                 , _infoVersion = toS $ showVersion Radicle.version
                 }
    swag = toSwagger daemonApi
    desc =
      "The radicle-daemon; a long-running background process which\
      \ materialises the state of remote IPFS machines on the users\
      \ PC, and writes to those IPFS machines the user is an owner of."
