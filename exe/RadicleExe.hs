module RadicleExe (main) where

import           API
import           Control.Lens ((^?))
import           Control.Monad.Catch (MonadThrow)
import           Data.Aeson (FromJSON, ToJSON, toJSON)
import           Data.Aeson.Lens (key, nth, _String)
import qualified Data.Text as T
import           GHC.Exts (fromList)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.Wreq as Wreq
import           Options.Applicative
import           Prelude (String)
import           Protolude hiding (TypeError, option, sourceFile)
import           Radicle
import           Servant.Client
import           System.Console.Haskeline (InputT)
import           System.Directory (doesFileExist)

import qualified Radicle.Internal.Number as Num
import qualified Radicle.Internal.PrimFns as PrimFns

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
         [String ipnsId, Vec v] -> do
             let values = T.intercalate "\n" $ renderCompactPretty <$> toList v
             liftIO (runExceptT $ sendIPFS ipnsId _ values)
             pure $ List []
         [_, Vec _] -> throwErrorHere $ TypeError "send!: first argument should be a string"
         [String _, _] -> throwErrorHere $ TypeError "send!: second argument should be a vector"
         xs     -> throwErrorHere $ WrongNumberOfArgs "send!" 2 (length xs)
      )
    receivePrimop =
      ( "receive!"
      , "Given a URL (string) and a integral number `n`, queries the remote chain\
        \ for the last `n` inputs that have been evaluated."
      , \case
          [String url, String lastKnown] ->
              liftIO (runExceptT $ receiveIPFS url lastKnown) >>= \case
                  Left err -> throwErrorHere . OtherError $ "receive!: " <> err
                  Right v -> pure $ Vec $ fromList v
          [String _, _] -> throwErrorHere
                         $ TypeError "receive!: expecting string as second arg"
          [_, _]        -> throwErrorHere
                         $ TypeError "receive!: expecting string as first arg"
          xs            -> throwErrorHere
                         $ WrongNumberOfArgs "receive!" 2 (length xs)
      )

-- * Client functions

-- | Send some values to IPFS, and update the IPNS link to the latest pointer.
-- Note that this does not check that the latest data is as it is known
-- locally, so this may lose data.
--
-- TODO: Currently this doesn't handle multiple keys! So it'll all go to your
-- default chain.
sendIPFS :: Text -> Text -> Text -> ExceptT Text IO Text
sendIPFS _ipnsId parent values = do
    let url1 = "http://localhost:5001/api/v0/dag/put"
    newDataRes <- liftIO $ Wreq.post url1 (toJSON $ IPFSBlock values (Just parent))
    cid <- case newDataRes ^? Wreq.responseBody . key "Cid" . key "/" . _String of
        Nothing -> throwError "couldn't add object"
        Just v  -> pure v
    let url2 = toS $ "http://localhost:5001/api/v0/name/publish?arg=" <> cid
    ipnsRes <- liftIO $ Wreq.get url2
    case ipnsRes ^? Wreq.responseBody . key "Name" . _String of
        Nothing -> throwError "couldn't update IPNS"
        Just v  -> pure v



-- | Receive a set of values from IPFS. The first argument is the IPNS name
-- (i.e., chain name). The second is the last value the client knows about.
-- (We can't just use an index because in theory it's possible for the chain
-- to have changed *prior* to that index.)
receiveIPFS :: Text -> Text -> ExceptT Text IO [Value]
receiveIPFS ipnsId lastKnown = do
    let url = "http://localhost:5001/api/v0/resolve?arg=" <> ipnsId
    res <- liftIO $ Wreq.get $ toS url
    cid <- case res ^? Wreq.responseBody . key "Path" . _String of
        Nothing -> throwError "nothing received"
        Just p  -> pure p
    getBlocks cid
  where
    getBlocks :: Text -> ExceptT Text IO [Value]
    getBlocks path = if path == lastKnown
        then pure []
        else do
            let url = "http://localhost:5001/api/v0/cat?arg=" <> path
            res <- liftIO $ Wreq.get (toS url) >>= Wreq.asJSON
            block <- case res ^? Wreq.responseBody of
                Nothing  -> throwError "not valid JSON"
                Just bl -> pure bl
            case parse "[ipfs]" (blockData block) of
                Left e -> throwError e
                Right v -> do
                    case blockParent block of
                        Nothing -> pure [v]
                        Just parent -> do
                            rest <- getBlocks parent
                            pure $ v : rest



-- | Chunks of data stored on IPFS. Essentially a linked list.
data IPFSBlock a = IPFSBlock
    { blockData :: a
    , blockParent :: Maybe Text
    } deriving (Eq, Show, Read, Generic, Functor)

instance FromJSON a => FromJSON (IPFSBlock a)
instance ToJSON a => ToJSON (IPFSBlock a)

submit :: [Value] -> ClientM Value
submit = client chainSubmitEndpoint . Values

since :: Int -> ClientM Value
since = client chainSinceEndpoint

runClientM' :: (MonadThrow m, MonadIO m) => Text -> HttpClient.Manager -> ClientM a -> m (Either ServantError a)
runClientM' baseUrl manager endpoint = do
    url <- parseBaseUrl $ T.unpack baseUrl
    liftIO $ runClientM endpoint $ mkClientEnv manager url
