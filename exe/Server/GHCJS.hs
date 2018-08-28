{-# LANGUAGE CPP #-}
module GHCJS where

import           Protolude hiding (TypeError)

import           API
import           Data.Scientific (floatingOrInteger)
import           GHC.Exts (fromList)
import           Paths_radicle
import           Radicle
import           Servant.API ((:<|>)(..))
import           Servant.Client.Ghcjs
import           System.Console.Haskeline (InputT)

main :: IO ()
main = do
    cfgSrc <- readFile =<< getDataFileName "rad/prelude.rad"
    repl Nothing cfgSrc bindings

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
