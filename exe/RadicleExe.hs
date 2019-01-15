module RadicleExe (main) where

import           Prelude (String)
import           Protolude hiding (TypeError, option, sourceFile)

import qualified Data.ByteString as BS
import           Options.Applicative
import           System.Directory (doesFileExist)

import           Radicle
import           Radicle.Internal.Effects (exitCode)
import           Radicle.Internal.MachineBackend.EvalServer
import           Radicle.Internal.MachineBackend.Ipfs (ipfsPrimFns)
import           Radicle.Internal.Pretty (putPrettyAnsi)

main :: IO ()
main = do
    opts' <- execParser allOpts
    code <-
        if sourceFile opts' == "-"
        then do
            src <- decodeUtf8With lenientDecode <$> BS.getContents
            let prog = interpretMany "[stdin]" src
            bindings <- createBindings (toS <$> scriptArgs opts')
            (result, _state) <- runLang bindings prog
            case result of
                Left (LangError _ (Exit n)) -> pure (exitCode n)
                Left e -> do putPrettyAnsi e
                             pure $ ExitFailure 1
                Right v -> do putPrettyAnsi v
                              pure ExitSuccess
        else do
            src <- ignoreShebang <$> readSource (sourceFile opts')
            hist <- case histFile opts' of
                Nothing -> getHistoryFile
                Just h  -> pure h
            bindings <- createBindings (toS <$> scriptArgs opts')
            repl (Just hist) (toS $ sourceFile opts') src bindings
    exitWith code
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc radDesc
       <> header "The radicle intepreter"
       <> noIntersperse
        )

readSource :: String -> IO Text
readSource file = do
    exists <- doesFileExist file
    if exists
    then decodeUtf8With lenientDecode <$> BS.readFile file
    else die $ "Could not find file: " <> toS file

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
    , scriptArgs :: [String]
    }

opts :: Parser Opts
opts = Opts
    <$> strArgument
        ( metavar "FILE"
       <> help "File to interpret. Use - to read the code from stdin."
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
    <*> many (strArgument mempty)

createBindings :: (MonadIO m, ReplM m) => [Text] -> IO (Bindings (PrimFns m))
createBindings scriptArgs' = do
    evalServerPackendPrimFns <- createEvalServerBackendPrimFns
    pure $ addPrimFns (replPrimFns scriptArgs' <> evalServerPackendPrimFns <> ipfsPrimFns) pureEnv
