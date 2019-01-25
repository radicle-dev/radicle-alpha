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

    , MachineId(..)
    , JsonValue(..)
    ) where

import           Protolude

import qualified Data.Aeson as A
import           Radicle.Daemon.Ipfs (JsonValue(..), MachineId(..))
import           Servant.API

instance FromHttpApiData MachineId where
    parseUrlPiece = Right . MachineId . toS
    parseQueryParam = parseUrlPiece

instance ToHttpApiData MachineId where
    toUrlPiece (MachineId id) = id

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
  } deriving (A.ToJSON, A.FromJSON)

newtype NewResult = NewResult
  { machineId :: MachineId
  } deriving (A.ToJSON, A.FromJSON)

-- * APIs

type Machine t = Capture "machineId" MachineId :> t
type Query = Machine ("query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression)
type Send  = Machine ("send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] SendResult)
type New   = "new"   :> Post '[JSON] NewResult

type DaemonApi =
  "v0" :> "machines" :> (Query :<|> Send :<|> New)

daemonApi :: Proxy DaemonApi
daemonApi = Proxy
