{-# LANGUAGE QuasiQuotes #-}

-- | This executable generates the radicle reference docs. This is a markdown
-- document which contains documentation for all the primitive functions and all
-- the functions defined in the prelude modules.
module ReferenceDoc (main) where

import           Protolude

import qualified Data.Default as Default
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Yaml hiding (Value)
import qualified GHC.Exts as GhcExts
import           Radicle
import           Radicle.Internal.Core
import           Radicle.Internal.Identifier
import           System.Console.Haskeline (defaultSettings, runInputT)
import           Text.Pandoc

data Content = Content
  { intro             :: Text
  , primFnsDoc        :: Text
  , preludeModulesDoc :: Text
  , primFns           :: [Text]
  , modules           :: [Text]
  } deriving (Generic)

instance FromJSON Content

main :: IO ()
main = do
    content <- decodeFileThrow "reference-doc.yaml"
    res_ <- runInputT defaultSettings $
             interpret
               "reference-doc"
               (   "(do"
                <> "(file-module! \"rad/prelude/test-eval.rad\") (import prelude/test-eval '[eval tests] :unqualified)"
                <> foldMap (\m -> "(file-module! \"rad/" <> m <> ".rad\")") (modules content)
                <> "(get-current-env))")
               replBindings
    let res = res_ `lPanic` "Error running the prelude."
    let s = bindingsFromRadicle res `lPanic` "Couldn't convert radicle state."
    let vars = GhcExts.toList (bindingsEnv s)
    checkAllDocumented [ fromIdent iden | (iden, Nothing, _) <- vars ]
    let e = Map.fromList [ (fromIdent iden, (docString, val)) | (iden, Just docString, val) <- vars ]
    checkAllInReference content e
    let rst = runPure (writeRST Default.def $ Pandoc nullMeta (doc content e)) `lPanic` "Couldn't generate RST"
    writeFile "docs/source/reference.rst" rst
  where

    doc content e =
      [ Header 1 nullAttr (parseInlineMarkdown "Radicle Reference")
      , Para $ parseInlineMarkdown (intro content)
      , Header 2 nullAttr (parseInlineMarkdown "Primitive functions")
      , Para $ parseInlineMarkdown $ primFnsDoc content
      ] ++ foldMap (valueDoc e) (primFns content) ++
      [ Header 2 nullAttr (parseInlineMarkdown "Prelude modules")
      , Para $ parseInlineMarkdown $ preludeModulesDoc content
      ] ++ foldMap (module' e) (modules content)

    defs v = let env = envFromRad v `lPanic` "Couldn't convert radicle value to as environment"
             in Map.fromList [ (fromIdent iden, (docString, val)) | (iden, Just docString, val) <- GhcExts.toList env ]

    module' env name = case lkp name env "Couldn't find module in the env" of
      (docString, Dict d) -> case (lkp [kword|exports|] d "invalid module", lkp [kword|env|] d "invalid module") of
        (Vec es, e) ->
          [ Header 2 nullAttr [ Code nullAttr (toS name) ]
          , Para (parseInlineMarkdown docString)
          ] ++ foldMap (export (defs e)) es
        _ -> panic $ "Module " <> name <> " didn't have an exports vec."
      _ -> panic $ "Module " <> name <> " was not a dict."

    export env (Atom (Ident name)) = valueDoc env name
    export _ _                     = panic "Export was not an atom."

    valueDoc env name =
        case lkp name env "couldn't find value" of
            (docString, Lambda args _ _) ->
                let callExample = "(" <> T.intercalate " " (name : map fromIdent args) <> ")"
                in [ Header 3 nullAttr [Code nullAttr (toS callExample) ] ]
                   <> parseMarkdownBlocks docString
            (docString, _) ->
                [ Header 3 nullAttr [Code nullAttr (toS name) ] ]
                <> parseMarkdownBlocks docString


    checkAllDocumented = \case
      [] -> pure ()
      vs -> panic $ "The following functions have no documentation strings: " <> T.intercalate ", " vs

    checkAllInReference content e =
      let notDocumented = Map.keys e \\ (primFns content ++ modules content ++ ["prelude/test-eval", "tests"]) in
      if null notDocumented
        then pure ()
      else panic $ "The following functions need to be added to the reference doc: " <> T.intercalate ", " notDocumented

    lPanic (Left e)  m = panic $ m <> ": " <> show e
    lPanic (Right r) _ = r

    lkp x m e = case Map.lookup x m of
      Just y  -> y
      Nothing -> panic $ e <> ": couldn't find " <> show x


parseInlineMarkdown :: Text -> [Inline]
parseInlineMarkdown t = case parseMarkdownBlocks t of
  [Para is] -> is
  _         -> panic $ "Expecting inline markdown but got blocks: " <> t

parseMarkdownBlocks :: Text -> [Block]
parseMarkdownBlocks t = case runPure (readCommonMark Default.def t) of
  Right (Pandoc _ block) -> block
  Left err               -> panic $ "Invalid markdown: " <> show err
