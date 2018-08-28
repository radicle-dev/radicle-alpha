{-# LANGUAGE CPP #-}
module GHCJS where

import           Protolude hiding (TypeError)

import           API
import           Data.IORef
import           Data.Scientific (floatingOrInteger)
import           GHC.Exts (fromList)
import GHCJS.Marshal
{-import           Paths_radicle-}
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(ThrowWouldBlock))
import JavaScript.Object (getProp, setProp)
import JavaScript.Object.Internal (Object(..))
import qualified Data.JSString as JSS
import qualified Data.Text as T
import           GHCJS.Types
import           Radicle
import           Servant.API ((:<|>)(..))
import           Servant.Client.Ghcjs
import           System.Console.Haskeline (InputT, defaultSettings, runInputT)
import           System.IO.Unsafe

main :: IO ()
main = do
    putStrLn ("Starting haskell" :: T.Text)
    cb <- syncCallback1 ThrowWouldBlock jsEval
    putStrLn ("here1" :: T.Text)
    js_set_eval cb
    putStrLn ("set" :: T.Text)
    -- do

    {-cfgSrc <- readFile =<< getDataFileName "rad/prelude.rad"-}
    {-repl Nothing cfgSrc bindings-}

-- * FFI

foreign import javascript unsafe "eval_fn_ = $1"
  js_set_eval :: Callback a -> IO ()

-- * Export

bndsRef :: IORef (Bindings (InputT IO))
bndsRef = unsafePerformIO $ newIORef bindings
{-# NOINLINE bndsRef #-}

jsEval :: JSVal -> IO ()
jsEval v = runInputT defaultSettings $ do
    let o = Object v
    putStrLn ("here" :: T.Text)
    ms <- liftIO $ fromJSVal =<< getProp "arg" o
    let s = case ms of
          Just s' -> s'
          _ -> error "blah"
    bnds <- liftIO $ readIORef bndsRef
    res <- runLang bnds $ interpretMany "[repl]" s
    out <- JSS.pack . T.unpack <$> case res of
        (Left err, _) -> pure $ renderPrettyDef err
        (Right v , newBnds) -> do
            _ <- liftIO $ writeIORef bndsRef newBnds
            pure $ renderPrettyDef v
    sout <- liftIO $ toJSVal out
    liftIO $ setProp "result" sout o



-- * Primops

bindings :: Bindings (InputT IO)
bindings = e { bindingsPrimops = bindingsPrimops e <> primops }
    where
      e :: Bindings (InputT IO)
      e = pureEnv

primops :: Primops (InputT IO)
primops = fromList [sendPrimop, receivePrimop] <> replPrimops
  where
    sendPrimop =
      ( Ident "send!"
      , evalArgs $ \case
         [String name, v] -> do
             res <- liftIO $ runClientM (submit $ List $ [String name, v])
             case res of
                 Left e   -> throwError . OtherError
                           $ "send!: failed:" <> show e
                 Right () -> pure $ List []
         [_, _] -> throwError $ TypeError "send!: first argument should be a string"
         xs     -> throwError $ WrongNumberOfArgs "send!" 2 (length xs)
      )
    receivePrimop =
      ( Ident "receive!"
      , evalArgs $ \case
          [String name, Number n] -> do
              case floatingOrInteger n of
                  Left (_ :: Float) -> throwError . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      liftIO (runClientM (since name r)) >>= \case
                          Left err -> throwError . OtherError
                                    $ "receive!: request failed:" <> show err
                          Right v' -> pure $ List v'
          [String _, _] -> throwError $ TypeError "receive!: expecting number as second arg"
          [_, _]        -> throwError $ TypeError "receive!: expecting string as first arg"
          xs            -> throwError $ WrongNumberOfArgs "receive!" 2 (length xs)
      )

-- * Helpers

identV :: Text -> Value
identV = Keyword . Ident

-- * Client functions

submit :: Value -> ClientM ()
since :: Text -> Int -> ClientM [Value]
submit :<|> since = client api
