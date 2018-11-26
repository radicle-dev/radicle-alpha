module RadicleExe (main) where

import           API
import           Control.Monad.Catch (MonadThrow)
import qualified Data.Text as T
import           GHC.Exts (fromList)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.HTTP.Client as HttpClient
import           Options.Applicative
import           Prelude (String)
import           Protolude hiding (TypeError, option, sourceFile)
import           Radicle
import           Servant.Client
import           System.Directory (doesFileExist)

import qualified Radicle.Internal.Number as Num
import qualified Radicle.Internal.PrimFns as PrimFns

main :: IO ()
main = do
    opts' <- execParser allOpts
    if sourceFile opts' == "-"
    then do
        src <- getContents
        mgr <- newManager defaultManagerSettings
        let prog = interpretMany (toS $ sourceFile opts') src
        (result, _state) <- runLang (bindings mgr) prog
        case result of
            Left (LangError _ Exit) -> pure ()
            Left e                  -> do putStrLn $ renderAnsi e
                                          exitWith (ExitFailure 1)
            Right v                 -> putStrLn $ renderAnsi v
    else do
        src <- readSource (sourceFile opts')
        hist <- case histFile opts' of
            Nothing -> getHistoryFile
            Just h  -> pure h
        mgr <- newManager defaultManagerSettings
        repl (Just hist) (toS $ sourceFile opts') src (bindings mgr)
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc radDesc
       <> header "The radicle intepreter"
        )

readSource :: String -> IO Text
readSource file = do
   exists <- doesFileExist file
   if exists
       then readFile file
       else die $ "Could not find file: " <> toS file

radDesc :: String
radDesc
    = "Interprets a radicle program.\n"
   <> "\n"
   <> "This program can also be used as a REPL by providing a file "
   <> "that defines a REPL. An example is the rad/repl.rad file included "
   <> "in the distribution."

-- * CLI Opts

data Opts = Opts
    { sourceFile :: FilePath
    , histFile   :: Maybe FilePath
    }

opts :: Parser Opts
opts = Opts
    <$> strArgument
        ( metavar "FILE"
       <> help "File to interpret. Use - to read the code from stdin."
        )
    <*> optional (strOption
        ( long "histfile"
       <> short 'H'
       <> metavar "FILE"
       <> help
           ( "File used to store the REPL history."
          <> "Defaults to $DIR/radicle/config.rad "
          <> "where $DIR is $XDG_DATA_HOME (%APPDATA% on Windows "
          <> "if that is set, or else ~/.local/share."
           )
        ))

-- * Primops

bindings :: (MonadIO m, ReplM m) => HttpClient.Manager -> Bindings (PrimFns m)
bindings mgr = addPrimFns (replPrimFns <> clientPrimFns mgr) pureEnv

clientPrimFns :: MonadIO m => HttpClient.Manager -> PrimFns m
clientPrimFns mgr = fromList . PrimFns.allDocs $ [sendPrimop, receivePrimop]
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
                      liftIO (runClientM' url mgr (since r)) >>= \case
                          Left err -> throwErrorHere . OtherError
                                    $ "receive!: request failed:" <> show err
                          Right v' -> pure v'
          (String _, v) -> throwErrorHere $ TypeError "receive!" 1 TNumber v
          (v, _)        -> throwErrorHere $ TypeError "receive!" 0 TString v
      )

-- * Client functions

submit :: [Value] -> ClientM Value
submit = client chainSubmitEndpoint . Values

since :: Int -> ClientM Value
since = client chainSinceEndpoint

runClientM' :: (MonadThrow m, MonadIO m) => Text -> HttpClient.Manager -> ClientM a -> m (Either ServantError a)
runClientM' baseUrl manager endpoint = do
    url <- parseBaseUrl $ T.unpack baseUrl
    liftIO $ runClientM endpoint $ mkClientEnv manager url
