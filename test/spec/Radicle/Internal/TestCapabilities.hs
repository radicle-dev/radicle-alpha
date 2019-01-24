-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities (
      runCodeWithWorld
    , runPureCode
    , runCodeWithInput
    , runCodeWithFiles

    , WorldState(..)
    , defaultWorldState
    ) where

import           Protolude hiding (TypeError)

import qualified Crypto.Random as CryptoRand
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import           System.Directory (getCurrentDirectory)
import qualified System.FilePath.Find as FP

import           Radicle
import           Radicle.Internal.Crypto
import           Radicle.Internal.Effects.Capabilities
import           Radicle.Internal.MachineBackend.Interface
import qualified Radicle.Internal.UUID as UUID


data WorldState = WorldState
    { worldStateStdin        :: [Text]
    , worldStateStdout       :: [Text]
    , worldStateFiles        :: Map Text Text
    , worldStateDRG          :: CryptoRand.ChaChaDRG
    , worldStateUUID         :: Int
    , worldStateRemoteChains :: Map Text (Seq.Seq Value)
    , worldStateCurrentTime  :: Time.UTCTime
    }

defaultWorldState :: WorldState
defaultWorldState =
    WorldState
    { worldStateStdin = []
    , worldStateStdout = []
    , worldStateFiles = mempty
    , worldStateDRG = CryptoRand.drgNewSeed (CryptoRand.seedFromInteger 4) -- chosen by fair dice roll
    , worldStateUUID = 0
    , worldStateRemoteChains = mempty
    , worldStateCurrentTime = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    }

worldStateWithSource :: IO WorldState
worldStateWithSource = do
    sources <- sourceFiles
    pure $ defaultWorldState { worldStateFiles = sources }

type TestLang = Lang (StateT WorldState IO)

-- | Run Radicle code in a REPL environment, with all library files
-- readable, and with the provided stdin lines.
--
-- The second item is the stdout.
runCodeWithInput :: [Text] -> Text -> IO (Either (LangError Value) Value, [Text])
runCodeWithInput input code = do
    ws' <- worldStateWithSource
    let ws = ws' { worldStateStdin = input }
    runCodeWithWorld ws code

-- | Run Radicle code in a REPL environment with the given files
-- readable.
runCodeWithFiles :: Map Text Text -> Text -> IO (Either (LangError Value) Value)
runCodeWithFiles files code =
    let ws = defaultWorldState { worldStateFiles = files }
    in fst <$> runCodeWithWorld ws code

-- | Run radicle code in a pure environment.
runPureCode :: Text -> Either (LangError Value) Value
runPureCode code =
    let prog = interpretMany "[test]" code
    in evalState (fst <$> runLang pureEnv prog) defaultWorldState

runCodeWithWorld :: WorldState -> Text -> IO (Either (LangError Value) Value, [Text])
runCodeWithWorld ws code = do
    let prog = interpretMany "[test]" code
    (result, ws') <- runStateT (fst <$> runLang testBindings prog) ws
    pure $ (result, worldStateStdout ws')

-- | Loads the content for all radicle sources in the "./rad" folder.
-- Returns a map from files names to contents.
sourceFiles :: IO (Map Text Text)
sourceFiles = do
    dir <- getCurrentDirectory
    absPaths <- FP.find FP.always (FP.extension FP.==? ".rad") dir
    filesWithContent <- forM absPaths $ \absPath -> do
        contents <- readFile absPath
        let path = drop (length dir + 5) absPath
        pure (toS path, contents)
    pure $ Map.fromList filesWithContent

-- | Bindings with REPL and client stuff mocked.
testBindings :: Bindings (PrimFns (StateT WorldState IO))
testBindings = addPrimFns memoryMachineBackendPrimFns (replBindings [])

-- | Provides a fake implementation of the @eval-server" machine
-- backend that store chains in a 'Map' in the 'WorldState'.
memoryMachineBackendPrimFns :: PrimFns (StateT WorldState IO)
memoryMachineBackendPrimFns =
    buildMachineBackendPrimFns MachineBackend
        { machineType = "eval-server"
        , machineUpdate =
            \id values -> do
                chains <- gets worldStateRemoteChains
                let exprs = Map.findWithDefault mempty id chains
                let newExprs = exprs Seq.>< values
                modify $ \s ->
                   s { worldStateRemoteChains = Map.insert id newExprs $ worldStateRemoteChains s }
                pure $ Right $ Seq.length newExprs
        , machineGetLog =
            \id maybeIndex -> do
                chains <- gets worldStateRemoteChains
                let exprs = Map.findWithDefault mempty id chains
                let index = fromMaybe 0 maybeIndex
                let newIndex = Seq.length exprs
                pure $ Right $ (newIndex, toList $ Seq.drop index exprs)
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

instance {-# OVERLAPPING #-} FileModule TestLang where
  fileModuleS fn = do
    fs <- lift $ gets worldStateFiles
    pure $ if Map.member fn fs then Just fn else Nothing

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
    -- 10 seconds go by every time the current time is queried.
    currentTime = do
      t <- gets worldStateCurrentTime
      modify $ \ws -> ws { worldStateCurrentTime = Time.addUTCTime 10 t }
      pure t

instance System (StateT WorldState IO) where
    systemS proc = lift $ systemS proc
    waitForProcessS = lift . waitForProcessS
    hPutStrS a b = lift $ hPutStrS a b
    hGetLineS = lift . hGetLineS
    hCloseS = lift . hCloseS
    openFileS a b = lift $ openFileS a b
