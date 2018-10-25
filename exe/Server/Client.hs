module Client where

import           API
import           Control.Monad.Catch (MonadThrow)
import           Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import           GHC.Exts (fromList)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.HTTP.Client as HttpClient
import           Options.Applicative
import           Protolude hiding (TypeError, option)
import           Radicle
import           Servant.Client
import           System.Console.Haskeline (InputT)

main :: IO ()
main = do
    opts' <- execParser allOpts
    cfgFile <- case configFile opts' of
        Nothing  -> getConfigFile
        Just cfg -> pure cfg
    cfgSrc <- readFile cfgFile
    hist <- case histFile opts' of
        Nothing -> getHistoryFile
        Just h  -> pure h
    mgr <- newManager defaultManagerSettings
    repl (Just hist) (toS cfgFile) cfgSrc (bindings mgr)
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run the radicle REPL"
       <> header "rad - The radicle REPL"
        )

-- * CLI Opts

data Opts = Opts
    { configFile :: Maybe FilePath
    , histFile   :: Maybe FilePath
    }

opts :: Parser Opts
opts = Opts
    <$> optional (strOption
        ( long "config"
       <> metavar "FILE"
       <> help "rad configuration file"
        ))
    <*> optional (strOption
        ( long "histfile"
       <> metavar "FILE"
       <> help "repl history file"
        ))

-- * Primops

bindings :: HttpClient.Manager -> Bindings (PrimFns (InputT IO))
bindings mgr = addPrimFns (replPrimFns <> clientPrimFns mgr) pureEnv

clientPrimFns :: HttpClient.Manager -> PrimFns (InputT IO)
clientPrimFns mgr = PrimFns (fromList [sendPrimop, receivePrimop])
  where
    sendPrimop =
      ( unsafeToIdent "send!"
      , \case
         [String url, v] -> do
             res <- liftIO $ runClientM' url mgr (submit v)
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
          [String url, Number n] -> do
              case floatingOrInteger n of
                  Left (_ :: Float) -> throwErrorHere . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      liftIO (runClientM' url mgr (since r)) >>= \case
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
submit = client chainSubmitEndpoint

since :: Int -> ClientM [Value]
since = client chainSinceEndpoint

runClientM' :: (MonadThrow m, MonadIO m) => Text -> HttpClient.Manager -> ClientM a -> m (Either ServantError a)
runClientM' baseUrl manager endpoint = do
    url <- parseBaseUrl $ T.unpack baseUrl
    liftIO $ runClientM endpoint $ mkClientEnv manager url
