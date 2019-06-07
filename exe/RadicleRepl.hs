module RadicleRepl (main) where

import           Prelude (String)
import           Protolude hiding (TypeError, option, sourceFile)

--import qualified Data.ByteString as BS
import           Options.Applicative
--import           System.Directory (doesFileExist)

import           Radicle
--import           Radicle.Internal.Effects (exitCode)
--import           Radicle.Internal.Pretty (putPrettyAnsi)

main :: IO ()
main = do
    opts' <- execParser allOpts
    hist <- case histFile opts' of
        Nothing -> getHistoryFile
        Just h  -> pure h
    bindings <- createImpureBindings []
    putStr intro
    repl (Just hist) "" bindings
  where
    allOpts = info (opts <**> helper)
        ( fullDesc
       <> progDesc radDesc
       <> header "The radicle REPL"
       <> noIntersperse
        )


intro :: Text
intro = "Radicle REPL\n\
    \\n\
    \Type (apropos!) for a list of functions in scope.\n\
    \Type (doc! '<name>) for further documentation of <name>.\n"

radDesc :: String
radDesc = "The radicle REPL"

newtype Opts = Opts
    { histFile   :: Maybe FilePath
    }

opts :: Parser Opts
opts = Opts
    <$> optional (strOption
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
