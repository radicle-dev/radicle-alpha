-- | This executable processes literate radicle files. It accepts Markdown, and
-- runs the code inside code blocks (within three backticks).
module Doc (main) where

import           Protolude

import qualified Data.Text as T
import           Options.Applicative
import           Radicle
import           Radicle.Internal
import           Text.Pandoc

main :: IO ()
main = do
    opts' <- execParser allOpts
    run (srcFile opts')
  where
    allOpts = info (opts <**> helper)
       ( fullDesc
      <> progDesc "Radicle literate file tester"
      <> header "radlit"
       )

run :: FilePath -> IO ()
run f = do
    txt <- readFile f
    let extensions = def { readerExtensions = githubMarkdownExtensions }
    pand <- runIOorExplode $ readMarkdown extensions txt
    let code = T.append "(load! (find-module-file! \"prelude.rad\"))" (getCode pand)
    bindings <- createImpureBindings []
    res <- runLang bindings $ interpretMany (toS f) $ code
    case res of
        (Left err, _) -> die . toS $ "Error: " ++ show err
        _             -> pure ()

getCode :: Pandoc -> Text
getCode (Pandoc _ blocks)
    = T.intercalate "\n\n" [ toS content | CodeBlock _ content <- blocks ]

newtype Opts = Opts
    { srcFile :: FilePath
    }

opts :: Parser Opts
opts = Opts <$> argument str (metavar "FILE")
