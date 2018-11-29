-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities where

import           Protolude hiding (TypeError)

import qualified Crypto.Random as CryptoRand
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import           GHC.Exts (fromList)
import qualified System.FilePath.Find as FP
import           System.Process (CreateProcess)

import           Radicle
import           Radicle.Internal.Core (addBinding)
import           Radicle.Internal.Crypto
import           Radicle.Internal.Effects.Capabilities
import qualified Radicle.Internal.Number as Num
import           Radicle.Internal.PrimFns (allDocs, twoArg)
import           Radicle.Internal.Type (Type(..))
import qualified Radicle.Internal.UUID as UUID

import           Paths_radicle


data WorldState = WorldState
    { worldStateStdin        :: [Text]
    , worldStateStdout       :: [Text]
    , worldStateEnv          :: Env Value
    , worldStateFiles        :: Map Text Text
    , worldStateDRG          :: CryptoRand.ChaChaDRG
    , worldStateUUID         :: Int
    , worldStateRemoteChains :: Map Text (Seq.Seq Value)
    , worldStateCurrentTime  :: Time.UTCTime
    , worldStateProcess      :: [(CreateProcess)]
    }


type TestLang = Lang (State WorldState)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWithFiles
    :: Bindings (PrimFns (State WorldState))
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
            , worldStateDRG = CryptoRand.drgNewSeed (CryptoRand.seedFromInteger 4) -- chosen by fair dice roll
            , worldStateUUID = 0
            , worldStateRemoteChains = mempty
            , worldStateCurrentTime = Time.UTCTime (Time.ModifiedJulianDay 0) 0
            , worldStateProcess = []
            }
    in case runState (fmap fst $ runLang bindings $ interpretMany "[test]" action) ws of
        (val, st) -> (val, worldStateStdout st)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: Bindings (PrimFns (State WorldState))
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
    :: Bindings (PrimFns (State WorldState))
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

-- | Bindings with REPL and client stuff mocked, and with -- a 'test-env__'
-- variable set to true.
testBindings :: Bindings (PrimFns (State WorldState))
testBindings
    = addBinding (unsafeToIdent "test-env__") Nothing (Boolean True)
    $ addPrimFns clientPrimFns replBindings

-- | Mocked versions of 'send!' and 'receive!'
clientPrimFns :: PrimFns (State WorldState)
clientPrimFns = fromList . allDocs $ [sendPrimop, receivePrimop]
  where
    sendPrimop =
      ( "send!"
      , "Mocked version of `send!` that stores sent values in a map with chains as keys."
      , twoArg "send!" $ \case
         (String url, Vec v) -> do
             lift . modify $ \s ->
                s { worldStateRemoteChains
                    = Map.insertWith (flip (Seq.><)) url v $ worldStateRemoteChains s }
             pure $ List []
         (String _, v) -> throwErrorHere $ TypeError "send!" 1 TVec v
         (v, _) -> throwErrorHere $ TypeError "send!" 0 TString v
      )
    receivePrimop =
      ( "receive!"
      , "Mocked version of `receive!` that retrieves values from a map with chains as keys."
      , twoArg "receive!" $ \case
          (String url, Number n) -> do
              case Num.isInt n of
                  Left _ -> throwErrorHere . OtherError
                                     $ "receive!: expecting int argument"
                  Right r -> do
                      chains <- lift $ gets worldStateRemoteChains
                      pure . List $ case Map.lookup url chains of
                              Nothing  -> []
                              Just res -> toList $ Seq.drop r res
          (String _, v) -> throwErrorHere $ TypeError "receive!" 1 TNumber v
          (v, _)        -> throwErrorHere $ TypeError "receive!" 0 TString v
      )

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

instance MonadRandom (State WorldState) where
    getRandomBytes i = do
        drg <- gets worldStateDRG
        let (a, drg') = CryptoRand.randomBytesGenerate i drg
        modify $ \ws -> ws { worldStateDRG = drg' }
        pure a

instance UUID.MonadUUID (State WorldState) where
    uuid = do
        i <- gets worldStateUUID
        modify $ \ws -> ws { worldStateUUID = i + 1 }
        pure . pad $ show i
      where
        prefix = "00000000-0000-0000-0000-"
        pad i = toS $ prefix <> replicate (12 - length i) '0' <> i

instance CurrentTime (State WorldState) where
  currentTime = gets worldStateCurrentTime

instance System (State WorldState) where
  systemS proc = do
     modify $ \ws -> ws { worldStateProcess = (proc):worldStateProcess ws }
     pure (Nothing, Nothing, ExitSuccess
