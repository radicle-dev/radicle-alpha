module Client where

import           API
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           GHC.Exts (fromList)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Options.Applicative
import           Protolude hiding (TypeError, option)
import           Radicle
import           Servant.API ((:<|>)(..))
import           Servant.Client
import           System.Console.Haskeline (InputT)

main :: IO ()
main = do
    opts' <- execParser allOpts
    cfgSrc <- readFile =<< case configFile opts' of
        Nothing -> getConfig
        Just cfg -> pure cfg
    hist <- case histFile opts' of
        Nothing -> getHistory
        Just h  -> pure h
    mgr <- newManager defaultManagerSettings
    let cEnv = mkClientEnv mgr (serverURL opts')
    repl hist cfgSrc (bindings cEnv)
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc "Run the radicle REPL"
       <> header "rad - The radicle REPL"
        )

-- * CLI Opts

data Opts = Opts
    { configFile  :: Maybe FilePath
    , serverURL   :: BaseUrl
    , histFile :: Maybe FilePath
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

bindings :: ClientEnv -> Bindings (InputT IO)
bindings cEnv = e { bindingsPrimops = bindingsPrimops e <> primops cEnv }
    where
      e :: Bindings (InputT IO)
      e = pureEnv

primops :: ClientEnv -> Primops (InputT IO)
primops cEnv = fromList [sendPrimop, receivePrimop] <> replPrimops
  where
    sendPrimop =
      ( Ident "send!"
      , evalArgs $ \case
         [String name, v] -> do
             res <- liftIO $ runClientM (submit $ List $ [String name, v]) cEnv
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
          [String name, Dict d] -> do
              st <- dictLookup "state" d ?? "receive!: expecting 'state' key"
              logs' <- dictLookup "logs" d ?? "receive!: expecting 'logs' key"
              results' <- dictLookup "results" d ?? "receive!: expecting 'results' key"
              logs <- case logs' of
                  List ls -> pure ls
                  _ -> throwError $ TypeError "receive!: 'logs' should be list"
              results <- case results' of
                  List ls -> pure ls
                  _ -> throwError $ TypeError "receive!: 'results' should be list"
              newLogs' <- liftIO
                        $ runClientM (since name $ length logs) cEnv
              newLogs <- case newLogs' of
                  Left err -> throwError . OtherError
                            $ "receive!: request failed:" <> show err
                  Right v' -> pure v'
              bnds' <- makeBindings st
              let (evalRes, bnds) = runIdentity $ runLang bnds'
                                  $ traverse eval newLogs
              case evalRes of
                 Left e -> throwError e
                 Right newResults -> do
                   let resDict = Dict $ Map.fromList
                           [ (identV "state", unmakeBindings bnds)
                           , (identV "logs", List $ logs ++ newLogs)
                           , (identV "results", List $ results ++ newResults)
                           ]
                   pure $ List [List newResults, resDict]
          [Keyword _, _] -> throwError $ TypeError "receive!: expecting dict as second arg"
          [_, _]      -> throwError $ TypeError "receive!: expecting keyword as first arg"
          xs -> throwError $ WrongNumberOfArgs "receive!" 2 (length xs)
      )

-- | Convert Bindings into a Value that can be used with radicle.
unmakeBindings :: Bindings m -> Value
unmakeBindings bnds = Dict $ Map.fromList
    [ (identV "env", Dict $ Map.mapKeys Atom $ fromEnv $ bindingsEnv bnds)
    , (identV "refs", List $ IntMap.elems (bindingsRefs bnds))
    ]

-- | Convert a value into Bindings, or throw an error.
makeBindings :: (Monad m, Monad n) => Value -> Lang m (Bindings n)
makeBindings val = case val of
    Dict d -> do
        env' <- dictLookup "env" d ?? "expecting 'env' key"
        refs' <- dictLookup "refs" d ?? "expecting 'refs' key"
        (nextRef, refs) <- makeRefs refs'
        env <- makeEnv env'
        pure $ Bindings env purePrimops refs nextRef
    _ -> throwError $ TypeError "expecting dict"
  where
    makeEnv env = case env of
        Dict d -> fmap (Env . Map.fromList)
                $ forM (Map.toList d) $ \(k, v) -> case k of
            Atom i -> pure (i, v)
            _      -> throwError $ TypeError "Expecting atom keys"
        _ -> throwError $ TypeError "Expecting dict"

    makeRefs refs = case refs of
        List ls -> pure (length ls, IntMap.fromList $ zip [0..] ls)
        _       -> throwError $ TypeError "Expecting dict"

-- * Helpers

identV :: Text -> Value
identV = Keyword . Ident

dictLookup :: Text -> Map Value Value -> Maybe Value
dictLookup key = Map.lookup (identV key)

-- | Throws an OtherError with the specified message if the Maybe is not a
-- Just.
(??) :: MonadError (LangError Value) m => Maybe a -> Text -> m a
a ?? msg = case a of
    Nothing -> throwError $ OtherError msg
    Just v  -> pure v

-- * Client functions

submit :: Value -> ClientM ()
since :: Text -> Int -> ClientM [Value]
submit :<|> since = client api
