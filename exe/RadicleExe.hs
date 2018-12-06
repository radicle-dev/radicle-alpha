module RadicleExe (main) where

import qualified Data.Text as T
import           GHC.Exts (fromList)
import           Prelude (String)
import           Protolude hiding (TypeError, option, sourceFile)

import           Options.Applicative
import           System.Directory (doesFileExist)

import           Radicle
import           Radicle.Internal.HttpStorage
import           Radicle.Internal.Pretty (putPrettyAnsi)
import           Radicle.Internal.PrimFns (allDocs)

main :: IO ()
main = do
    opts' <- execParser allOpts
    if sourceFile opts' == "-"
    then do
        src <- getContents
        let prog = interpretMany (toS $ sourceFile opts') src
        bindings <- createBindings (toS <$> scriptArgs opts')
        (result, _state) <- runLang bindings prog
        case result of
            Left (LangError _ Exit) -> pure ()
            Left e                  -> do putPrettyAnsi e
                                          exitWith (ExitFailure 1)
            Right v                 -> putPrettyAnsi v
    else do
        src <- ignoreShebang <$> readSource (sourceFile opts')
        hist <- case histFile opts' of
            Nothing -> getHistoryFile
            Just h  -> pure h
        bindings <- createBindings (toS <$> scriptArgs opts')
        repl (Just hist) (toS $ sourceFile opts') src bindings
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
       then readFile file
       else die $ "Could not find file: " <> toS file

-- | Since `#` is not a comment in radicle, we need to explictly remove shebang
-- lines.
ignoreShebang :: Text -> Text
ignoreShebang src = case T.lines src of
    f:rest -> T.unlines $ if "#!" `T.isPrefixOf` f then rest else f:rest
    _      -> src

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

argsPrimFn :: forall m . Monad m => [Text] -> PrimFns m
argsPrimFn args = fromList $ allDocs
    [ ( "get-args!"
      , "Returns the list of the command-line arguments the script was called with"
      , \case
          [] -> pure . Vec $ fromList (String <$> args)
          xs -> throwErrorHere $ WrongNumberOfArgs "get-args!" 0 (length xs)
      )
    ]

createBindings :: (MonadIO m, ReplM m) => [Text] -> IO (Bindings (PrimFns m))
createBindings scriptArgs' = do
    storage <- createHttpStoragePrimFns
    pure $ addPrimFns (argsPrimFn scriptArgs' <> replPrimFns <> storage) pureEnv
