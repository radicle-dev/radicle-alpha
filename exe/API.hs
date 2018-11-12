{-# OPTIONS_GHC -fno-warn-orphans #-}
module API where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           GHC.Exts (IsList(..))
import           Protolude
import           Radicle
import           Radicle.Internal.Annotation (thisPos)
import           Servant.API

instance MimeRender PlainText Value where
    mimeRender _ x = LBS.fromStrict $ encodeUtf8 $ renderCompactPretty x

instance MimeUnrender PlainText Value where
    mimeUnrender _ x =
        let src = decodeUtf8 $ LBS.toStrict x
        in first T.unpack $ parse "[request body]" src

newtype Values = Values { getValues :: [Value] }
    deriving (Eq, Show, Ord)

instance IsList Values where
    type Item Values = Value
    fromList = Values
    toList = getValues

instance MimeRender PlainText Values where
    mimeRender _ (Values xs)
        = LBS.intercalate "\n" $ mimeRender p <$> xs
      where
         p = Proxy :: Proxy PlainText

instance MimeUnrender PlainText Values where
    mimeUnrender _ x =
        let src = decodeUtf8 $ LBS.toStrict x
        in case parseValues "[request body]" src of
            Right vs -> Right $ Values vs
            Left err -> Left . toS $ renderPrettyDef err'
              where err' :: LangError Value
                    err' = LangError [thisPos] (ParseError err)

-- | Endpoint to submit an expression to a chain. Responds with @:ok@ if the
-- expression has been submitted sucessfully.
type ChainSubmitEndpoint = "submit" :> ReqBody '[PlainText] Values :> Post '[PlainText] Value

chainSubmitEndpoint :: Proxy ChainSubmitEndpoint
chainSubmitEndpoint = Proxy

-- | Endpoint to obtain all entries in a chain starting from the given
-- index. Responds with a Radicle vector of the expressions.
type ChainSinceEndpoint = "since" :> Capture "index" Int :> Get '[PlainText] Value

chainSinceEndpoint :: Proxy ChainSinceEndpoint
chainSinceEndpoint = Proxy
