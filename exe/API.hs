{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Protolude
import           Radicle
import           Servant.API

instance MimeRender PlainText Value where
    mimeRender _ x = LBS.fromStrict $ encodeUtf8 $ renderCompactPretty x

instance MimeUnrender PlainText Value where
    mimeUnrender _ x =
        let src = decodeUtf8 $ LBS.toStrict x
        in first T.unpack $ parse "[request body]" src

-- | Endpoint to submit an expression to a chain. Responds with @:ok@ if the
-- expression has been submitted sucessfully.
type ChainSubmitEndpoint = "submit" :> ReqBody '[PlainText] Value :> Post '[PlainText] Value

chainSubmitEndpoint :: Proxy ChainSubmitEndpoint
chainSubmitEndpoint = Proxy

-- | Endpoint to obtain all entries in a chain starting from the given
-- index. Responds with a Radicle vector of the expressions.
type ChainSinceEndpoint = "since" :> Capture "index" Int :> Get '[PlainText] Value

chainSinceEndpoint :: Proxy ChainSinceEndpoint
chainSinceEndpoint = Proxy
