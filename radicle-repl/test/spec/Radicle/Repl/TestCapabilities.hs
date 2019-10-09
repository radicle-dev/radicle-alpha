-- | This module defines instances for the classes in
-- "Radicle.Repl.Capabilities" that may be used for testing.
module Radicle.Repl.TestCapabilities (
      runCodeWithWorld
    , runCodeWithFiles

    , WorldState(..)
    , defaultWorldState
    ) where

import           Protolude hiding (TypeError)

import qualified Crypto.Random as CryptoRand
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Time as Time

import           Radicle
import           Radicle.Internal.Crypto
import qualified Radicle.Internal.UUID as UUID
import           Radicle.Repl (replPrimFns)
import           Radicle.Repl.Capabilities


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

type TestLang = Lang (StateT WorldState IO)

-- | Run Radicle code in a REPL environment with the given files
-- readable.
runCodeWithFiles :: Map Text Text -> Text -> IO (Either (LangError Value) Value)
runCodeWithFiles files code =
    let ws = defaultWorldState { worldStateFiles = files }
    in fst <$> runCodeWithWorld ws code

runCodeWithWorld :: WorldState -> Text -> IO (Either (LangError Value) Value, [Text])
runCodeWithWorld ws code = do
    let prog = interpretMany "[test]" code
        bindings = addPrimFns (replPrimFns []) pureEnv
    (result, ws') <- runStateT (fst <$> runLang bindings prog) ws
    pure $ (result, worldStateStdout ws')

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
    setCurrentDirS = lift . setCurrentDirS
