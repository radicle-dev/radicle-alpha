{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import           Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import           Network.HTTP.Media ((//))
import           Protolude
import           Radicle
import           Servant.API
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

data CBOR

instance Accept CBOR where
    contentType _ = "application" // "cbor"

instance Serialise a => MimeRender CBOR a where
    mimeRender _ x = serialise x

instance Serialise a => MimeUnrender CBOR a where
    mimeUnrender _ x = first show (deserialiseOrFail x)

instance Show a => MimeRender PlainText a where
    mimeRender _ x = show x

instance Read a => MimeUnrender PlainText a where
    mimeUnrender _ x = readEither $ T.unpack $ decodeUtf8 $ BSL.toStrict x

{-data HsBin-}

{-instance Accept HsBin where-}
    {-contentType _ = "application" // "x-hs-bin"-}

{-instance Binary a => MimeRender HsBin a where-}
    {-mimeRender _ x = encode x-}

{-instance Binary a => MimeUnrender HsBin a where-}
    {-mimeUnrender _ x = case decodeOrFail x of-}
      {-Left (_,_,err) -> Left err-}
      {-Right (_,_,v) -> Right v-}

type API
  =    "submit" :> ReqBody '[PlainText] Value :> Post '[PlainText] ()
  :<|> "since"  :> Capture "chain" Text :> Capture "index" Int :> Get '[PlainText] [Value]
  :<|> Raw

api :: Proxy API
api = Proxy
