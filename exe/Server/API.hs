{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Protolude
import           Radicle
import           Servant.API

instance Show a => MimeRender PlainText a where
    mimeRender _ x = show x

instance Read a => MimeUnrender PlainText a where
    mimeUnrender _ x = readEither $ T.unpack $ decodeUtf8 $ LBS.toStrict x


type ChainSubmitEndpoint = "submit" :> ReqBody '[PlainText] Value :> Post '[PlainText] ()

chainSubmitEndpoint :: Proxy ChainSubmitEndpoint
chainSubmitEndpoint = Proxy

type ChainSinceEndpoint = "since" :> Capture "index" Int :> Get '[PlainText] [Value]

chainSinceEndpoint :: Proxy ChainSinceEndpoint
chainSinceEndpoint = Proxy
