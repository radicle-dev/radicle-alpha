-- | This executable processes literate radicle files. It accepts Markdown, and
-- runs the code inside code blocks (within three backticks).
module Doc where

import Protolude
import Radicle
import Pandoc
import Options.Applicative

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
    pand <- runCommonMark def f
    runLang replBindings $ interpretMany f $ getCode pand

getCode :: Pandoc -> Text
getCode (Pandoc blocks)
    = mconcat [ content | CodeBlock attr content <- block, isRad attr ]
  where
    isRad (ident, _, _) = ident == "radicle"

data Opts = Opts
    { srcFile :: FilePath
    }

opts :: Parser Opts
opts = Opts <$> argument str (metavar "FILE")
