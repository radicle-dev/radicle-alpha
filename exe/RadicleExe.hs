module RadicleExe (main) where

import           Prelude (String)
import           Protolude hiding (TypeError, option, sourceFile)

import qualified Data.ByteString as BS
import           Options.Applicative
import           System.Directory (doesFileExist)

import           Radicle
import           Radicle.Internal.Effects (exitCode)
import           Radicle.Internal.Pretty (putPrettyAnsi)

main :: IO ()
main = do
    opts' <- execParser allOpts
    bindings <- createImpureBindings (toS <$> scriptArgs opts')
    code <-
        if sourceFile opts' == "-"
        then do
            src <- decodeUtf8With lenientDecode <$> BS.getContents
            script "[stdin]" src bindings
        else do
            let srcFile = sourceFile opts'
            src <- readSource srcFile
            script (toS srcFile) src bindings
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
radDesc = "Interprets a radicle program."

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
