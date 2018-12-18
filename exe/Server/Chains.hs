module Server.Chains
    ( logInfo

    , Chains
    , updateChain
    , getChainLog

    , prepareDatabase
    ) where

import           Protolude hiding (fromStrict, option)

import           Control.Monad.Except
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Database.PostgreSQL.Simple (Connection)
import qualified STMContainers.Map as StmMap

import           Radicle
import           Server.DB

data Chain = Chain
    { chainName      :: Text
    , chainState     :: Bindings (PrimFns Identity)
    , chainEvalPairs :: Seq.Seq (Value, Value)
    } deriving (Generic)

newChain :: Text -> Chain
newChain name = Chain name pureEnv mempty

type Chains = StmMap.Map Text (MVar Chain)

-- | Returns the inputs submitted to a given chain. If the chain does
-- not exist we return an empty list
getChainLog :: MonadIO m => Connection -> Text -> Int -> m [Value]
getChainLog conn name index = liftIO $ getSinceDB conn name index

-- | Evaluate a list of expressions in the given chain and update the
-- chain with the new state.
updateChain :: (MonadIO m) => Connection -> Chains -> Text -> [Value] -> m (Either Text Int)
updateChain conn chains name vals = liftIO $ runExceptT $ do
    chainM <- loadChain conn chains name
    tryModifyMVar chainM $ \chain -> do
        chain' <- liftEither (updateChain' chain vals)
            `withLoggedError` \_ -> ("Input evaluation failed", [("machine", name)])
        liftIO $ traverse_ (insertExprDB conn name) vals
        pure $ (chain', Seq.length (chainEvalPairs chain') - 1)

-- | Evaluates inputs against the current chain state and return the
-- chains state with the accepted inputs.
--
-- Returns an error if the evaluation failed.
updateChain' :: Chain -> [Value] -> Either Text Chain
updateChain' chain vals =
    let (result, newSt) = runIdentity $ runLang (chainState chain) $ traverse eval vals
    in case result of
        Left e  -> do
            Left $ renderPrettyDef e
        Right output -> do
             let news = Seq.fromList $ zip vals output
             Right $ chain { chainState = newSt
                           , chainEvalPairs = chainEvalPairs chain Seq.>< news
                           }

-- | Return a chain stored in memory in the 'Chains' argument. If there
-- is no chain for the given name loaded in memory we load it from the
-- database. This requires evaluation and might throw an error
loadChain :: Connection -> Chains -> Text -> ExceptT Text IO (MVar Chain)
loadChain conn chainsM name = do
    (chainM, isNew) <- getChainLock chainsM name
    when isNew $
        tryModifyMVar_ chainM $ \_ -> do
            values <- liftIO $ getSinceDB conn name (-1)
            liftEither $ updateChain' (newChain name) values
    pure $ chainM

-- | The second item indicates whether we created the chain lock and
-- tells the caller to load the chain from the database.
getChainLock :: MonadIO m => Chains -> Text -> m (MVar Chain, Bool)
getChainLock chainsM name = liftIO $ do
    newChainM <- newMVar $ newChain name
    atomically $ do
        existingChain <- StmMap.lookup name chainsM
        case existingChain of
            Just c -> pure (c, False)
            Nothing -> do
                StmMap.insert newChainM name chainsM
                pure (newChainM, True)

-- | Log a message to @stdout@.
--
-- The second argument is a list of key-value pairs that will be joined with "="
-- and appended to the message.
--
-- @
--      logInfo "the message" [("foo", "5)]
--      -- prints "INFO   the message foo=5"
-- @
logInfo :: MonadIO m => Text -> [(Text, Text)] -> m ()
logInfo msg dat = do
    putStrLn $ "INFO   " <> msg <> datString
  where
    datString = T.intercalate "" $ map (\(key, val) -> " " <> key <> "=" <> val) dat

withLoggedError :: ExceptT e IO a -> (e -> (Text, [(Text, Text)])) -> ExceptT e IO a
withLoggedError action makeLog =
    action `catchError` \e -> do
        liftIO $ uncurry logInfo (makeLog e)
        throwError e

--
-- Helpers
--

tryModifyMVar :: MVar a -> (a -> ExceptT e IO (a, r)) -> ExceptT e IO r
tryModifyMVar mvar f =
    ExceptT $ modifyMVar mvar $ \a -> do
        result <- runExceptT $ f a
        pure $ case result of
            Left e        -> (a, Left e)
            Right (a', r) -> (a', Right r)

tryModifyMVar_ :: MVar a -> (a -> ExceptT e IO a) -> ExceptT e IO ()
tryModifyMVar_ mvar f =
    tryModifyMVar mvar $ \a -> do
        a' <- f a
        pure (a', ())
