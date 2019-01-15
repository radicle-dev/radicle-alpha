{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daemon (main) where

import           Protolude hiding (fromStrict, option)

import qualified Data.Aeson as A
import           Network.Wai.Handler.Warp (run)
import           Servant

import           Server.Common

import           Radicle
import           Radicle.Internal.MachineBackend.Ipfs

newtype Expression = Expression { expression :: Value }
  deriving (Generic)

instance A.FromJSON Expression where
  parseJSON = notImplemented

instance A.ToJSON Expression where
  toJSON = notImplemented

newtype Expressions = Expressions { expressions :: [Value] }
  deriving (Generic)

instance A.FromJSON Expressions where
  parseJSON = notImplemented

instance A.ToJSON Expressions where
  toJSON = notImplemented

type Query = "query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression
type Send  = "send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] ()
type New   = "new"   :> Post '[JSON] IpnsId

type DaemonApi =
  "machines" :> ( Capture "machineId" Text :> ( Query :<|> Send ) :<|> New )

serverApi :: Proxy DaemonApi
serverApi = Proxy

-- * Main

main :: IO ()
main = do
    let port = 8909 -- TODO(james) decide how/where/if port can be confugured
    let app = serve serverApi server
    logInfo "Start listening" [("port", show port)]
    run port app

server :: Server DaemonApi
server = machineEndpoints :<|> newMachine
  where
    newMachine :: Handler IpnsId = pure "thisisanIPNSid" -- TODO(james)
    machineEndpoints id = query id :<|> send id
    query id (Expression v) = pure $ Expression $ List [String id, v] -- TODO(james)
    send _id (Expressions _vs) = pure () -- TODO(james)
