{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Daemon.HttpApi
    ( Query
    , Send
    , SendResult(..)
    , NewResult(..)
    , New
    , DaemonApi
    , daemonApi
    , Expressions(..)
    , Expression(..)
    , swagger
    ) where

import           Protolude

import qualified Data.Aeson as A
import           Data.Swagger
import           Data.Version (showVersion)
import           Radicle.Daemon.Ipfs (JsonValue, MachineId(..))
import           Servant.API
import           Servant.Swagger

import qualified Paths_radicle as Radicle

instance FromHttpApiData MachineId where
  parseUrlPiece = Right . MachineId . toS
  parseQueryParam = parseUrlPiece

newtype Expression = Expression { expression :: JsonValue }
  deriving (Generic)

instance A.ToJSON Expression
instance A.FromJSON Expression

newtype Expressions = Expressions { expressions :: [JsonValue] }
  deriving (Generic)

instance A.FromJSON Expressions
instance A.ToJSON Expressions

newtype SendResult = SendResult
  { results :: [JsonValue]
  } deriving (Generic)

instance A.ToJSON SendResult

newtype NewResult = NewResult
  { machineId :: MachineId
  } deriving (Generic)

instance A.ToJSON NewResult

-- * APIs

type Query = "query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression
type Send  = "send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] SendResult
type New   = "new"   :> Post '[JSON] NewResult

type DaemonApi =
  "v0" :> "machines" :> ( Capture "machineId" MachineId :> ( Query :<|> Send ) :<|> New )

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

instance ToSchema JsonValue where
  declareNamedSchema _ = pure $
    NamedSchema (Just "RadicleValue") $
      mempty { _schemaDescription = Just "A radicle value formatted according to radicle's S-expression syntax."
             , _schemaParamSchema = mempty {_paramSchemaType = SwaggerString } }

instance ToSchema Expression
instance ToSchema Expressions

instance ToSchema SendResult
instance ToSchema NewResult

swagger :: A.Value
swagger = A.toJSON $
  (toSwagger daemonApi) { _swaggerInfo = mempty { _infoTitle = "Radicle daemon"
                                                , _infoDescription = Just desc
                                                , _infoVersion = toS $ showVersion Radicle.version
                                                } }
  where
    desc =
      "The radicle-daemon; a long-running background process which\
      \ materialises the state of remote IPFS machines on the users\
      \ PC, and writes to those IPFS machines the user is an owner of."
