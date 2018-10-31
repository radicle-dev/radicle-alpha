{-# LANGUAGE QuasiQuotes #-}

module Client where

import           API
import           Data.Scientific (floatingOrInteger)
import           GHC.Exts (fromList)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Options.Applicative
import           Protolude hiding (TypeError, option)
import           Radicle
import           Servant.API ((:<|>)(..))
import           Servant.Client
import           System.Console.Haskeline (InputT)

import           Radicle.Internal.Doc (md)
import qualified Radicle.Internal.PrimFns as PrimFns

main :: IO ()
main = do
    opts' <- execParser allOpts
    cfgFile <- case configFile opts' of
        Nothing  -> getConfig
        Just cfg -> pure cfg
    cfgSrc <- readFile cfgFile
    hist <- case histFile opts' of
        Nothing -> getHistory
        Just h  -> pure h
    mgr <- newManager defaultManagerSettings
    let cEnv = mkClientEnv mgr (serverURL opts')
    repl (Just hist) (toS cfgFile) cfgSrc (bindings cEnv)
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run the radicle REPL"
       <> header "rad - The radicle REPL"
        )

-- * CLI Opts

data Opts = Opts
    { configFile :: Maybe FilePath
    , serverURL  :: BaseUrl
    , histFile   :: Maybe FilePath
    }

opts :: Parser Opts
opts = Opts
    <$> optional (strOption
        ( long "config"
       <> metavar "FILE"
       <> help "rad configuration file"
        ))
    <*> option (str >>= parseBaseUrl')
        ( long "url"
       <> metavar "URL"
       <> help "URL of server"
        )
    <*> optional (strOption
        ( long "histfile"
       <> metavar "FILE"
       <> help "repl history file"
        ))
  where
    parseBaseUrl' x = case parseBaseUrl x of
        Nothing -> readerError "can't parse URL"
        Just v  -> pure v

-- * Primops

bindings :: ClientEnv -> Bindings (PrimFns (InputT IO))
bindings cEnv
    = e { bindingsPrimFns = bindingsPrimFns e <> prims
        , bindingsEnv = bindingsEnv e <> primFnsEnv prims
        }
  where
    e :: Bindings (PrimFns (InputT IO))
    e = replBindings
    prims = primops cEnv

primops :: ClientEnv -> PrimFns (InputT IO)
primops cEnv = fromList $ PrimFns.allDocs $ [sendPrimop, receivePrimop]
  where
    sendPrimop =
      ( "send!"
      , [md|Given a URL (string) and a value, sends the value `v` to the remote
           chain located at the URL for evaluation.|]
      , \case
         [String name, v] -> do
             res <- liftIO $ runClientM (submit $ List $ [String name, v]) cEnv
             case res of
                 Left e   -> throwErrorHere . OtherError
                           $ "send!: failed:" <> show e
                 Right () -> pure $ List []
         [_, _] -> throwErrorHere $ TypeError "send!: first argument should be a string"
         xs     -> throwErrorHere $ WrongNumberOfArgs "send!" 2 (length xs)
      )
    receivePrimop =
      ( "receive!"
      , [md|Given a URL and a integer `n`, queries the remote chain for the last `n`
           inputs that have been evaluated.|]
      , \case
          [String name, Number n] -> do
              case floatingOrInteger n of
                  Left (_ :: Float) -> throwErrorHere . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      liftIO (runClientM (since name r) cEnv) >>= \case
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
