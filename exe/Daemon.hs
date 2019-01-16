{-# OPTIONS_GHC -fno-warn-orphans #-}

module Daemon where

import           Protolude hiding (fromStrict, option)

import           Control.Monad.Fail
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (fromStrict)
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified Network.HTTP.Types.URI as URL

import qualified Radicle.Internal.UUID as UUID
import           Server.Common

import           Radicle
import qualified Radicle.Internal.MachineBackend.Ipfs as Ipfs

newtype IpnsId = IpnsId Text deriving (A.ToJSON)

instance FromHttpApiData IpnsId where
  parseUrlPiece = Right . IpnsId . toS . URL.urlDecode False . toS
  parseQueryParam = parseUrlPiece

newtype JsonValue = JsonValue Value

instance A.FromJSON JsonValue where
  parseJSON = A.withText "JsonValue" $ \t -> do
      v <- case parse "[daemon]" t of
        Left err -> fail $ "failed to parse Radicle expression: " <> show err
        Right v  -> pure v
      pure (JsonValue v)

instance A.ToJSON JsonValue where
  toJSON (JsonValue v) = A.String $ renderCompactPretty v

newtype Expression = Expression { expression :: JsonValue }
  deriving (Generic)

instance A.ToJSON Expression
instance A.FromJSON Expression

newtype Expressions = Expressions { expressions :: [JsonValue] }
  deriving (Generic)

instance A.FromJSON Expressions
instance A.ToJSON Expressions

type Query = "query" :> ReqBody '[JSON] Expression  :> Post '[JSON] Expression
type Send  = "send"  :> ReqBody '[JSON] Expressions :> Post '[JSON] ()
type New   = "new"   :> Post '[JSON] IpnsId

type DaemonApi =
  "machines" :> ( Capture "machineId" IpnsId :> ( Query :<|> Send ) :<|> New )

serverApi :: Proxy DaemonApi
serverApi = Proxy

-- * Main

main :: IO ()
main = do
    let port = 8909 -- TODO(james): decide how/where/if port can be confugured
    let app = serve serverApi server
    logInfo "Start listening" [("port", show port)]
    run port app

server :: Server DaemonApi
server = machineEndpoints :<|> newMachine
  where
    machineEndpoints id = query id :<|> send id

-- * Methods

newMachine :: Handler IpnsId
newMachine = do
  m_ <- liftIO $ Ipfs.ipfsMachineCreate =<< UUID.uuid
  case m_ of
    Left err -> throwError $ err500 { errBody = fromStrict $ encodeUtf8 err }
    Right id -> pure (IpnsId id)

query :: IpnsId -> Expression -> Handler Expression
query (IpnsId id) (Expression (JsonValue v)) = pure $ Expression $ JsonValue $ List [String id, v] -- TODO(james):

send :: IpnsId -> Expressions -> Handler ()
send _id (Expressions _vs) = pure ()
