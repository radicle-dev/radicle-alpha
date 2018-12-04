-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities (
      sourceFiles
    , runTestWithFiles
    , runTest
    , testBindings
    , runTest'
    , runTestWith
    ) where

import           Protolude hiding (TypeError)

import qualified Crypto.Random as CryptoRand
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import           System.Directory (getCurrentDirectory)
import qualified System.FilePath.Find as FP
import           System.IO.Unsafe (unsafePerformIO)

import           Radicle
import           Radicle.Internal.Core (addBinding)
import           Radicle.Internal.Crypto
import           Radicle.Internal.Effects.Capabilities
import           Radicle.Internal.Storage
import qualified Radicle.Internal.UUID as UUID


data WorldState = WorldState
    { worldStateStdin        :: [Text]
    , worldStateStdout       :: [Text]
    , worldStateEnv          :: Env Value
    , worldStateFiles        :: Map Text Text
    , worldStateDRG          :: CryptoRand.ChaChaDRG
    , worldStateUUID         :: Int
    , worldStateRemoteChains :: Map Text (Seq.Seq Value)
    , worldStateCurrentTime  :: Time.UTCTime
    }


type TestLang = Lang (StateT WorldState IO)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWithFiles
    :: Bindings (PrimFns (StateT WorldState IO))
    -> [Text]  -- The stdin (errors if it runs out)
    -> Map Text Text -- The files
    -> Text -- The program
    -> IO (Either (LangError Value) Value, [Text])
runTestWithFiles bindings inputs files action = do
    let ws = WorldState
            { worldStateStdin = inputs
            , worldStateStdout = []
            , worldStateEnv = bindingsEnv bindings
            , worldStateFiles = files
            , worldStateDRG = CryptoRand.drgNewSeed (CryptoRand.seedFromInteger 4) -- chosen by fair dice roll
            , worldStateUUID = 0
            , worldStateRemoteChains = mempty
            , worldStateCurrentTime = Time.UTCTime (Time.ModifiedJulianDay 0) 0
            }
    (val, st) <- runStateT (fmap fst $ runLang bindings $ interpretMany "[test]" action) ws
    pure (val, worldStateStdout st)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: Bindings (PrimFns (StateT WorldState IO))
    -> [Text]  -- The stdin (errors if it runs out)
    -> Text -- The program
    -> IO (Either (LangError Value) Value, [Text])
runTestWith bindings inputs action = runTestWithFiles bindings inputs mempty action

-- | Run a test without stdin/stdout
runTest
    :: Bindings (PrimFns (StateT WorldState IO))
    -> Text
    -> IO (Either (LangError Value) Value)
runTest bnds prog = fst <$> runTestWith bnds [] prog

-- | Like 'runTest', but uses the pureEnv
runTest' :: Text -> Either (LangError Value) Value
runTest' a = unsafePerformIO $ runTest pureEnv a


-- | Loads the content for all radicle sources in the "./rad" folder.
-- Returns a map from files names to contents.
sourceFiles :: IO (Map Text Text)
sourceFiles = do
    dir <- getCurrentDirectory
    absPaths <- FP.find FP.always (FP.extension FP.==? ".rad") dir
    filesWithContent <- forM absPaths $ \absPath -> do
        contents <- readFile absPath
        let path = drop (length dir + 1) absPath
        pure (toS path, contents)
    pure $ Map.fromList filesWithContent

-- | Bindings with REPL and client stuff mocked, and with -- a 'test-env__'
-- variable set to true.
testBindings :: Bindings (PrimFns (StateT WorldState IO))
testBindings
    = addBinding (unsafeToIdent "test-env__") Nothing (Boolean True)
    $ addPrimFns storagePrimFns replBindings


-- | Provides @send!@ and @receive!@ functions that store chains in a
-- 'Map' in the 'WorldState'.
storagePrimFns :: PrimFns (StateT WorldState IO)
storagePrimFns =
    buildStoragePrimFns StorageBackend
        { storageSend =
            ( "send!"
            , "Mocked version of `send!` that stores sent values in a map with chains as keys."
            , \id values -> do
                modify $ \s ->
                   s { worldStateRemoteChains
                       = Map.insertWith (flip (Seq.><)) id values $ worldStateRemoteChains s }
                pure $ Right ()
            )
        , storageReceive =
            ( "receive!"
            , "Mocked version of `receive!` that retrieves values from a map with chains as keys."
            , \id index -> do
                chains <- gets worldStateRemoteChains
                let exprs = case Map.lookup id chains of
                        Nothing  -> []
                        Just res -> toList $ Seq.drop index res
                pure $ Right $ List exprs
            )
        }

instance {-# OVERLAPPING #-} Stdin TestLang where
    getLineS = do
        ws <- lift get
        case worldStateStdin ws of
            []   -> pure Nothing
            h:hs -> lift (put $ ws { worldStateStdin = hs }) >> pure (Just h)

instance {-# OVERLAPPING #-} Stdout TestLang where
    putStrS t = lift $
        modify (\ws -> ws { worldStateStdout = t:worldStateStdout ws })
    supportsANSI = pure False

instance {-# OVERLAPPING #-} ReadFile TestLang where
  readFileS fn = do
    fs <- lift $ gets worldStateFiles
    case Map.lookup fn fs of
        Just f  -> pure $ Right f
        Nothing -> throwErrorHere . OtherError $ "File not found: " <> fn

instance Monad m => MonadRandom (StateT WorldState m) where
    getRandomBytes i = do
        drg <- gets worldStateDRG
        let (a, drg') = CryptoRand.randomBytesGenerate i drg
        modify $ \ws -> ws { worldStateDRG = drg' }
        pure a

instance Monad m => UUID.MonadUUID (StateT WorldState m) where
    uuid = do
        i <- gets worldStateUUID
        modify $ \ws -> ws { worldStateUUID = i + 1 }
        pure . pad $ show i
      where
        prefix = "00000000-0000-0000-0000-"
        pad i = toS $ prefix <> replicate (12 - length i) '0' <> i

instance CurrentTime (StateT WorldState IO) where
    currentTime = gets worldStateCurrentTime

instance System (StateT WorldState IO) where
    systemS proc = lift $ systemS proc
    waitForProcessS = lift . waitForProcessS
    hPutStrS a b = lift $ hPutStrS a b
    hGetLineS = lift . hGetLineS
