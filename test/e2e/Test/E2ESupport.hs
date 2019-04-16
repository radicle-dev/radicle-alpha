-- | This module provides functions for defining tests, making assertions, and
-- interacting with the test environment. Import this module instead of
-- "Test.Tasty" and its submodules.
--
-- We export a subset of the "Test.Tasty.HUnit" functions lifted to 'TestM'
module Test.E2ESupport
    ( TestM
    , projectDir
    , using
    , RadDaemon(..)

    , testCaseSteps
    , testCase

    , prepareRadicle

    , runTestCommand
    , runTestCommand'
    , runTestCommandForError

    , TestTree
    , assertEqual
    , assertContains
    , assertAbsence
    ) where

import           Protolude hiding (bracket)

import           Control.Exception.Safe ()
import           Control.Monad.Catch (bracket)
import qualified Data.Text as T
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Tasty
import qualified Test.Tasty.HUnit as HUnit

type TestM = ReaderT TestEnv IO

data TestEnv = TestEnv
    { homeDir    :: FilePath
    , projectDir :: FilePath
    }

data RadDaemon = RadDaemon1 | RadDaemon2
    deriving (Eq, Show, Read)

-- | Prepares 'TestEnv' by creating a temporary directory that serves as the
-- home directory, then runs 'TestM' with the environment.
--
-- We also adjust the @PATH@ environment variable so that it includes the
-- projects @bin@ folder.
runTestM :: TestM a -> IO a
runTestM testM = do
    projectDir <- getCurrentDirectory
    setRadPath (projectDir </> "rad")
    withSystemTempDirectory "radicle-test1" $ \dir ->
        let env = TestEnv
                { homeDir = dir
                , projectDir = projectDir
                }
        in withCurrentDirectory dir
            $ withExtendedSearchPath (projectDir </> "bin")
            $ runReaderT testM env
  where
    -- | Runs the given action with a modified search path that includes @path@.
    withExtendedSearchPath :: FilePath -> IO a -> IO a
    withExtendedSearchPath path action = bracket (prependSearchPath path) setSearchPath (const action)

    -- | Prepend the path to the search path and return the original search path
    prependSearchPath :: FilePath -> IO [FilePath]
    prependSearchPath path = do
        originalSearchPath <- getSearchPath
        setSearchPath (path : originalSearchPath)
        pure originalSearchPath

    setSearchPath :: [FilePath] -> IO ()
    setSearchPath paths = setEnv "PATH" $ intercalate [searchPathSeparator] paths

    setRadPath :: FilePath -> IO ()
    setRadPath path = setEnv "RADPATH" path

-- | Run a command against a particular radicle daemon. Behaves like `local`,
-- so nesting a `using` will override for the computation.
--
-- Example:
-- > using RadDaemon1 $ runTestCommand ...
using :: RadDaemon -> TestM a -> TestM a
using rd act
    = bracket (liftIO $ lookupEnv apiPath >>= \old -> setEnv apiPath (envFor rd) >> pure old)
              (\old -> liftIO $ case old of
                  Nothing -> unsetEnv apiPath
                  Just o' -> setEnv apiPath o')
              (const act)
  where
    apiPath = "RAD_DAEMON_API_URL"
    envFor RadDaemon1 = "http://localhost:19302"
    envFor RadDaemon2 = "http://localhost:19303"

-- * Lift test definitions and assertions to 'TestM'

testCaseSteps :: TestName -> ((Text -> TestM ()) -> TestM ()) -> TestTree
testCaseSteps name mkTest =
    HUnit.testCaseSteps name $ \step ->
        let step' = liftIO . step . toS
        in runTestM (mkTest step')

testCase :: TestName -> TestM () -> TestTree
testCase name test =
    HUnit.testCase name $ runTestM test


assertEqual :: (HasCallStack, MonadIO m, Eq a, Show a) => Text -> a -> a -> m ()
assertEqual msg a a' = liftIO $ HUnit.assertEqual (toS msg) a a'

assertFailure :: (HasCallStack, MonadIO m) => Text -> m a
assertFailure msg = liftIO $ HUnit.assertFailure $ toS msg

-- | @assertContains str substr@ throws as assertion error if @substr@ is not
-- contained in @str@
assertContains :: (HasCallStack, MonadIO m) => Text -> Text -> m ()
assertContains str substr =
    if substr `T.isInfixOf` str
    then pure ()
    else assertFailure $ "\"" <> substr <> "\" is not contained in \"" <> str <> "\""

-- | @assertAbsence str substr@ throws as assertion error if @substr@ is
-- contained in @str@
assertAbsence :: (HasCallStack, MonadIO m) => Text -> Text -> m ()
assertAbsence str substr =
    if substr `T.isInfixOf` str
    then assertFailure $ "\"" <> substr <> "\" should not be contained in \"" <> str <> "\""
    else pure ()

-- * Setup

-- | Runs @rad key create@ and sets the Git user name and email.
prepareRadicle :: TestM Text
prepareRadicle = do
    _ <- using RadDaemon1 $ runTestCommand "rad-key" ["create"]
    _ <- runTestCommand "git" ["config", "--global", "user.name", "Alice"]
    runTestCommand "git" ["config", "--global", "user.email", "alice@example.com"]

-- * Run commands

-- | Like 'runTestCommand'' but does not provide input to stdin.
runTestCommand :: (HasCallStack) => FilePath -> [Text] -> TestM Text
runTestCommand bin args = runTestCommand' bin args []

-- | Run a command with the given arguments and stdin lines and return stdout.
--
-- The command is executed with @HOME@ set to 'homeDir' from the test
-- environment.
--
-- Any trailing newlines are stripped from the output.
--
-- If the command exists with a non-zero exit code an exception is
-- thrown.
runTestCommand' :: (HasCallStack) => FilePath -> [Text] -> [Text] -> TestM Text
runTestCommand' bin args inputLines = do
    (exitCode, out, err) <- executeCommand bin args inputLines
    case exitCode of
        ExitSuccess -> pure $ trimNewline out
        ExitFailure _ ->
            let commandLine = toS bin <> " " <> T.unwords args
            in assertFailure $
                "Command failed: " <> commandLine <> "\n"
                <> "-- stdout ---------\n"
                <> out
                <> "-- stderr ---------\n"
                <> err
                <> "-------------------\n"

-- | Like 'runTestCommandForError'' but does not provide input to stdin.
runTestCommandForError :: (HasCallStack) => FilePath -> [Text] -> TestM Text
runTestCommandForError bin args = runTestCommandForError' bin args []

-- | Like 'runTestCommand'' but expects the command to return an error.
--
-- If the command exits with a non-zero exit code (as expected), stdout
-- of the command is returned. Otherwise, an exception is thrown.
runTestCommandForError' :: (HasCallStack) => FilePath -> [Text] -> [Text] -> TestM Text
runTestCommandForError' bin args inputLines = do
    (exitCode, out, err) <- executeCommand bin args inputLines
    case exitCode of
        ExitFailure _ -> pure $ trimNewline out
        ExitSuccess ->
            let commandLine = toS bin <> " " <> T.unwords args
            in assertFailure $
                "Command " <> commandLine <> " succeeded,\n"
                <> "but was expected to fail." <> "\n"
                <> "-- stdout ---------\n"
                <> out
                <> "-- stderr ---------\n"
                <> err
                <> "-------------------\n"

executeCommand :: FilePath -> [Text] -> [Text] -> TestM (ExitCode, Text, Text)
executeCommand bin args inputLines = do
    let argsString = map toS args
    TestEnv {..} <- ask
    searchPath <- liftIO $ getEnv "PATH"
    radDaemon <- liftIO $ fromMaybe "" <$> lookupEnv "RAD_DAEMON_API_URL"
    let env = [ ("PATH", searchPath)
              , ("HOME", homeDir)
              , ("RAD_DAEMON_API_URL", radDaemon)
              , ("RADPATH", projectDir </> "rad")
              ]
    let procSpec = (proc bin argsString) { env = Just env }
    let input = T.unpack $ T.intercalate "\n" inputLines
    (code, stout, sterr) <- liftIO $ readCreateProcessWithExitCode procSpec input
    pure (code, toS stout, toS sterr)

trimNewline :: Text -> Text
trimNewline = T.dropWhileEnd (== '\n')
