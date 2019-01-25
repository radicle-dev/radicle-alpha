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
    ) where

import           Protolude

import qualified Data.Aeson as A
import           Radicle.Daemon.Ipfs (JsonValue, MachineId(..))
import           Servant.API

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
  } deriving (A.ToJSON)

newtype NewResult = NewResult
  { machineId :: MachineId
  } deriving (A.ToJSON)

-- * APIs

type Query = "query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression
type Send  = "send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] SendResult
type New   = "new"   :> Post '[JSON] NewResult

type DaemonApi =
  "v0" :> "machines" :> ( Capture "machineId" MachineId :> ( Query :<|> Send ) :<|> New )

daemonApi :: Proxy DaemonApi
daemonApi = Proxy
