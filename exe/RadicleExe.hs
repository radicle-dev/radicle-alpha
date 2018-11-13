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
import           System.Console.Haskeline (InputT)
import           System.Directory (doesFileExist)

import qualified Radicle.Internal.PrimFns as PrimFns
import qualified Radicle.Internal.Number as Num

main :: IO ()
main = do
    opts' <- execParser allOpts
    src <- do
       exists <- doesFileExist (sourceFile opts')
       if exists
           then readFile (sourceFile opts')
           else die $ "Could not find file: " <> toS (sourceFile opts')
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
       <> help "File to interpret."
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

bindings :: HttpClient.Manager -> Bindings (PrimFns (InputT IO))
bindings mgr = addPrimFns (replPrimFns <> clientPrimFns mgr) pureEnv

clientPrimFns :: HttpClient.Manager -> PrimFns (InputT IO)
clientPrimFns mgr = fromList . PrimFns.allDocs $ [sendPrimop, receivePrimop]
  where
    sendPrimop =
      ( "send!"
      , "Given a URL (string) and a value, sends the value `v` to the remote\
        \ chain located at the URL for evaluation."
      , \case
         [String url, Vec v] -> do
             res <- liftIO $ runClientM' url mgr (submit $ toList v)
             case res of
                 Left e   -> throwErrorHere . OtherError
                           $ "send!: failed:" <> show e
                 Right r  -> pure r
         [_, Vec _] -> throwErrorHere $ TypeError "send!: first argument should be a string"
         [String _, _] -> throwErrorHere $ TypeError "send!: second argument should be a vector"
         xs     -> throwErrorHere $ WrongNumberOfArgs "send!" 2 (length xs)
      )
    receivePrimop =
      ( "receive!"
      , "Given a URL (string) and a integral number `n`, queries the remote chain\
        \ for the last `n` inputs that have been evaluated."
      , \case
          [String url, Number n] -> do
              case Num.isInt n of
                  Left _ -> throwErrorHere . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      liftIO (runClientM' url mgr (since r)) >>= \case
                          Left err -> throwErrorHere . OtherError
                                    $ "receive!: request failed:" <> show err
                          Right v' -> pure v'
          [String _, _] -> throwErrorHere $ TypeError "receive!: expecting number as second arg"
          [_, _]        -> throwErrorHere $ TypeError "receive!: expecting string as first arg"
          xs            -> throwErrorHere $ WrongNumberOfArgs "receive!" 2 (length xs)
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
