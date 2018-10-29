{-# OPTIONS_GHC -fno-warn-deprecations #-}
module GHCJS where

import           Protolude hiding (TypeError)

import           API
import           Data.IORef
import qualified Data.JSString as JSS
import           Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import           GHC.Exts (fromList)
import           GHCJS.Foreign.Callback (Callback, OnBlocked(..), syncCallback1)
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object (getProp, setProp)
import           JavaScript.Object.Internal (Object(..))
import           Radicle
import           Servant.API ((:<|>)(..))
import           Servant.Client.Ghcjs
import           System.Console.Haskeline (InputT, defaultSettings, runInputT)

main :: IO ()
main = do
    bndsRef <- newIORef bindings
    res <- runInputT defaultSettings
        $ runLang bindings
        $ interpretMany "[repl]" "(load! \"rad/prelude.rad\")"
    case res of
        (Left err, _)      -> liftIO $ print $ renderPrettyDef err
        (Right _, newBnds) -> liftIO $ writeIORef bndsRef newBnds
    cb <- syncCallback1 ContinueAsync (jsEval bndsRef)
    js_set_eval cb

-- * FFI

foreign import javascript unsafe "eval_fn_ = $1"
  js_set_eval :: Callback a -> IO ()

-- * Export

-- | Interop with JS is quite nasty. We take a JS object, expecting the 'arg'
-- property to be set to the argument, and we set the 'result' property of it
-- with the result.
--
-- The IORef keeps the state/env of the REPL so far.
jsEval :: IORef (Bindings (PrimFns (InputT IO))) -> JSVal -> IO ()
jsEval bndsRef v = runInputT defaultSettings $ do
    let o = Object v
    ms <- liftIO $ fromJSVal =<< getProp "arg" o
    let s = case ms of
          Just s' -> s'
          _       -> panic "expecting 'arg' key"
    bnds <- liftIO $ readIORef bndsRef
    res <- runLang bnds $ interpretMany "[repl]" s
    out <- JSS.pack . T.unpack <$> case res of
        (Left err, _) -> pure $ renderPrettyDef err
        (Right v', newBnds) -> do
            _ <- liftIO $ writeIORef bndsRef newBnds
            pure $ renderPrettyDef v'
    sout <- liftIO $ toJSVal out
    liftIO $ setProp "result" sout o

-- * Primops

bindings :: Bindings (PrimFns (InputT IO))
bindings = e { bindingsPrimFns = bindingsPrimFns e <> primops }
    where
      e :: Bindings (PrimFns (InputT IO))
      e = pureEnv

primops :: PrimFns (InputT IO)
primops = PrimFns (fromList [sendPrimop, receivePrimop]) <> replPrimFns
  where
    sendPrimop =
      ( unsafeToIdent "send!"
      , \case
         [String name, v] -> do
             res <- liftIO $ runClientM (submit $ List $ [String name, v])
             case res of
                 Left e   -> throwErrorHere . OtherError
                           $ "send!: failed:" <> show e
                 Right () -> pure $ List []
         [_, _] -> throwErrorHere $ TypeError "send!: first argument should be a string"
         xs     -> throwErrorHere $ WrongNumberOfArgs "send!" 2 (length xs)
      )
    receivePrimop =
      ( unsafeToIdent "receive!"
      , \case
          [String name, Number n] -> do
              case floatingOrInteger n of
                  Left (_ :: Float) -> throwErrorHere . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      liftIO (runClientM (since name r)) >>= \case
                          Left err -> throwErrorHere . OtherError
                                    $ "receive!: request failed:" <> show err
                          Right v' -> pure $ List v'
          [String _, _] -> throwErrorHere $ TypeError "receive!: expecting number as second arg"
          [_, _]        -> throwErrorHere $ TypeError "receive!: expecting string as first arg"
          xs            -> throwErrorHere $ WrongNumberOfArgs "receive!" 2 (length xs)
      )

-- * Helpers

identV :: Text -> Value
identV = Keyword . unsafeToIdent

-- * Client functions

submit :: Value -> ClientM ()
since :: Text -> Int -> ClientM [Value]
submit :<|> since :<|> _ = client api
