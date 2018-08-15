module API where

import           Codec.Serialise (Serialise, deserialise, serialise)
import           Network.HTTP.Media ((//))
import           Protolude
import           Radicle
import           Servant.API

data CBOR

instance Accept CBOR where
    contentType _ = "application" // "cbor"

instance Serialise a => MimeRender CBOR a where
    mimeRender _ x = serialise x

instance Serialise a => MimeUnrender CBOR a where
    mimeUnrender _ x = Right $ deserialise x

type API
  =    "submit" :> ReqBody '[CBOR] Value :> Post '[CBOR] ()
  :<|> "since"  :> Capture "chain" Text :> Capture "index" Int :> Get '[CBOR] [Value]

api :: Proxy API
api = Proxy
