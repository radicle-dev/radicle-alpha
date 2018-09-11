{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import           Protolude
import           Radicle
import           Servant.API
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Aeson as A

instance Show a => MimeRender PlainText a where
    mimeRender _ x = show x

instance Read a => MimeUnrender PlainText a where
    mimeUnrender _ x = readEither $ T.unpack $ decodeUtf8 $ LBS.toStrict x

type API
  =    "submit" :> ReqBody '[PlainText] Value :> Post '[PlainText] ()
  :<|> "since"  :> Capture "chain" Text :> Capture "index" Int :> Get '[PlainText] [Value]
  :<|> "outputs" :> Capture "chain" Text :> Get '[JSON, PlainText] [Maybe A.Value]
  :<|> Raw

api :: Proxy API
api = Proxy
