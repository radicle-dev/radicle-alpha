{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Provide Radicle primitives for storing chains through an HTTP API.
module Radicle.Internal.HttpStorage
    ( ChainSubmitEndpoint
    , chainSubmitEndpoint
    , ChainSinceEndpoint
    , chainSinceEndpoint
    , Values(..)

    , createHttpStoragePrimFns
    ) where

import           Protolude hiding (TypeError, toList)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           GHC.Exts (IsList(..))
import qualified Network.HTTP.Client as HttpClient
import           Servant.API
import           Servant.Client

import           Radicle
import           Radicle.Internal.Annotation (thisPos)
import qualified Radicle.Internal.Number as Num
import qualified Radicle.Internal.PrimFns as PrimFns


--
-- * API
--

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


--
-- * Client
--

-- | Returns definitions for the primitive functions
-- @send!@ and @receive@.
createHttpStoragePrimFns :: MonadIO m => IO (PrimFns m)
createHttpStoragePrimFns = do
    httpMgr <- HttpClient.newManager HttpClient.defaultManagerSettings
    pure $ httpStoragePrimFns' httpMgr

httpStoragePrimFns' :: MonadIO m => HttpClient.Manager -> PrimFns m
httpStoragePrimFns' mgr = fromList . PrimFns.allDocs $ [sendPrimop, receivePrimop]
  where
    sendPrimop =
      ( "send!"
      , "Given a URL (string) and a value, sends the value `v` to the remote\
        \ chain located at the URL for evaluation."
      , PrimFns.twoArg "send!" $ \case
         (String url, Vec v) -> do
             res <- liftIO $ runClientM' url mgr (submit $ toList v)
             case res of
                 Left e   -> throwErrorHere . OtherError
                           $ "send!: failed:" <> show e
                 Right r  -> pure r
         (String _, v) -> throwErrorHere $ TypeError "send!" 1 TVec v
         (v, _) -> throwErrorHere $ TypeError "send!" 0 TString v
      )
    receivePrimop =
      ( "receive!"
      , "Given a URL (string) and a integral number `n`, queries the remote chain\
        \ for the last `n` inputs that have been evaluated."
      , PrimFns.twoArg "receive!" $ \case
          (String url, Number n) -> do
              case Num.isInt n of
                  Left _ -> throwErrorHere . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      res <- runClientM' url mgr (since r)
                      case res of
                          Left err -> throwErrorHere . OtherError
                                    $ "receive!: request failed:" <> show err
                          Right v' -> pure v'
          (String _, v) -> throwErrorHere $ TypeError "receive!" 1 TNumber v
          (v, _)        -> throwErrorHere $ TypeError "receive!" 0 TString v
      )

submit :: [Value] -> ClientM Value
submit = client chainSubmitEndpoint . Values

since :: Int -> ClientM Value
since = client chainSinceEndpoint

runClientM' :: (MonadIO m) => Text -> HttpClient.Manager -> ClientM a -> m (Either ServantError a)
runClientM' baseUrl manager endpoint = liftIO $ do
    url <- parseBaseUrl $ T.unpack baseUrl
    liftIO $ runClientM endpoint $ mkClientEnv manager url
