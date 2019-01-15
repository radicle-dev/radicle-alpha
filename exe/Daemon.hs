module Daemon (main) where

import           Protolude hiding (fromStrict, option)

import           Servant
import           Network.Wai.Handler.Warp (run)

import           Server.Common

import           Radicle
import           Radicle.Internal.MachineBackend.EvalServer
import           Radicle.Internal.MachineBackend.Ipfs

type Query = "query" :> ReqBody '[PlainText] Value  :> Post '[PlainText] Value
type Send  = "send"  :> ReqBody '[PlainText] Values :> Post '[JSON] ()
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
    query id v = pure $ List [String id, v] -- TODO(james)
    send _id _vs = pure () -- TODO(james)
