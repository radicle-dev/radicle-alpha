-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities where

import           Protolude

import qualified Crypto.Random as CryptoRand
import qualified Data.Map.Strict as Map
import qualified System.FilePath.Find as FP

import           Radicle
import           Radicle.Internal.Crypto
import           Radicle.Internal.Effects.Capabilities

import           Paths_radicle

data WorldState = WorldState
    { worldStateStdin  :: [Text]
    , worldStateStdout :: [Text]
    , worldStateEnv    :: Env Value
    , worldStateFiles  :: Map Text Text
    , worldDRG         :: CryptoRand.ChaChaDRG
    }


type TestLang = Lang (State WorldState)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWithFiles
    :: Bindings (Primops (State WorldState))
    -> [Text]  -- The stdin (errors if it runs out)
    -> Map Text Text -- The files
    -> Text -- The program
    -> (Either (LangError Value) Value, [Text])
runTestWithFiles bindings inputs files action =
    let ws = WorldState
            { worldStateStdin = inputs
            , worldStateStdout = []
            , worldStateEnv = bindingsEnv bindings
            , worldStateFiles = files
            , worldDRG = CryptoRand.drgNewSeed (CryptoRand.seedFromInteger 4) -- chosen by fair dice roll
            }
    in case runState (fmap fst $ runLang bindings $ interpretMany "[test]" action) ws of
        (val, st) -> (val, reverse $ worldStateStdout st)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: Bindings (Primops (State WorldState))
    -> [Text]  -- The stdin (errors if it runs out)
    -> Text -- The program
    -> (Either (LangError Value) Value, [Text])
runTestWith bindings inputs action = runTestWithFiles bindings inputs mempty action

-- | Like `runTestWith`, but uses the pureEnv
runTestWith'
    :: [Text]
    -> Text
    -> (Either (LangError Value) Value, [Text])
runTestWith' = runTestWith pureEnv

-- | Run a test without stdin/stdout
runTest
    :: Bindings (Primops (State WorldState))
    -> Text
    -> Either (LangError Value) Value
runTest bnds prog = fst $ runTestWith bnds [] prog

-- | Like 'runTest', but uses the pureEnv
runTest' :: Text -> Either (LangError Value) Value
runTest' = runTest pureEnv

-- | The radicle source files, along with their directory.
sourceFiles :: IO (FilePath, [FilePath])
sourceFiles = do
    dir <- getDataDir
    allFiles <- FP.find FP.always (FP.extension FP.==? ".rad") dir
    pure (dir <> "/", drop (length dir + 1) <$> allFiles)


instance {-# OVERLAPPING #-} Stdin TestLang where
    getLineS = do
        ws <- lift get
        case worldStateStdin ws of
            []   -> throwErrorHere Exit
            h:hs -> lift (put $ ws { worldStateStdin = hs }) >> pure h

instance {-# OVERLAPPING #-} Stdout TestLang where
    putStrS t = lift $
        modify (\ws -> ws { worldStateStdout = t:worldStateStdout ws })

instance {-# OVERLAPPING #-} ReadFile TestLang where
  readFileS fn = do
    fs <- lift $ gets worldStateFiles
    case Map.lookup fn fs of
      Just f  -> pure f
      Nothing -> throwErrorHere . OtherError $ "File not found: " <> fn

instance MonadRandom (State WorldState) where
    getRandomBytes i = do
      drg <- gets worldDRG
      let (a, drg') = CryptoRand.randomBytesGenerate i drg
      modify $ \ws -> ws { worldDRG = drg' }
      pure a
