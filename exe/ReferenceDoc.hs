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
import           Radicle
import qualified Radicle.Daemon.HttpApi as Daemon
import           Radicle.Internal.Core
import qualified Radicle.Internal.Doc as Doc
import           Radicle.Internal.Identifier
import           Radicle.TH
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
    bindings <- createImpureBindings []
    res_ <- interpret
               "reference-doc"
               (   "(do"
                <> "(file-module! \"prelude/test-macro.rad\") (import prelude/test-macro '[test tests] :unqualified)"
                <> foldMap (\m -> "(file-module! \"" <> m <> ".rad\")") (modules content)
                <> "(get-current-state))")
               bindings
    let res = res_ `lPanic` "Error running the prelude."
    let env = bindingsEnv $ bindingsFromRadicle res `lPanic` "Couldn't convert radicle state."
    let rst = runPure (writeRST Default.def $ Pandoc nullMeta (doc content env)) `lPanic` "Couldn't generate RST"
    writeFile "docs/source/reference.rst" rst
    writeFile "docs/source/daemon-api.yaml" (toS (encode Daemon.swagger))
  where

    doc :: Content -> Env Value -> [Block]
    doc content env =
      [ Header 1 nullAttr (parseInlineMarkdown "Radicle Reference")
      , Para $ parseInlineMarkdown (intro content)
      ] ++ docForPrimFns content ++
      [ Header 2 nullAttr (parseInlineMarkdown "Prelude modules")
      , Para $ parseInlineMarkdown $ preludeModulesDoc content
      ] ++ foldMap (moduleDoc env) (modules content)

moduleDoc :: Env Value -> Text -> [Block]
moduleDoc (Env env) name =
    case Map.lookup (Ident name) env of
        Nothing -> panic $ "Couldn’t find module " <> name
        Just (Doc.Docd Nothing _) -> panic $ "Module " <> name <> " is not documented"
        Just (Doc.Docd (Just docString) (Dict module')) ->
            header docString <> concatMap (exportsDoc name (getModuleEnv module')) (getExports module')
        _ -> panic $ "Module " <> name <> " was not a dict."
  where
    header :: Text -> [Block]
    header docString =
        [ Header 2 nullAttr [ Code nullAttr (toS name) ]
        , Para (parseInlineMarkdown docString) ]

    getExports :: Map Value Value -> Seq Text
    getExports module' =
        case lkp [kword|exports|] module' of
            (Vec exports) -> flip map exports $ \case Atom (Ident n) -> n
                                                      _ -> panic $ "Export in module " <> name <> " is not an atom"
            _ -> panic $ "Exports of module " <> name <> " are not a vector"

    getModuleEnv :: Map Value Value -> Env Value
    getModuleEnv module' =
        case lkp [kword|env|] module' of
          VEnv e -> e
          _      -> panic "Module's `:env` was not an env."

    lkp :: Value -> Map Value Value -> Value
    lkp x m = case Map.lookup x m of
      Just y  -> y
      Nothing -> panic $ "Invalid module " <> name <> ": couldn't find key " <> show x


exportsDoc :: Text -> Env Value -> Text -> [Block]
exportsDoc moduleName_ env name =
    case Map.lookup (Ident name) (fromEnv env) of
        Nothing -> panic $ "Unknown export " <> name <> "in module " <> moduleName_
        Just (Doc.Docd Nothing _) -> panic $ "Missing documentation for " <> name <> " in " <> moduleName_
        Just (Doc.Docd (Just docString) value) -> valueDoc name docString value

lPanic :: Show a => Either a p -> Text -> p
lPanic (Left e)  m = panic $ m <> ": " <> show e
lPanic (Right r) _ = r

valueDoc :: Text -> Text -> Value -> [Block]
valueDoc name docString value =
  [ Header 3 nullAttr [Code nullAttr (toS title) ] ]
  <> parseMarkdownBlocks docString
  where
    title = fromMaybe name $ callExample name value

-- | Generate documentation for primitive functions defined by 'replBindings'
docForPrimFns :: Content -> [Block]
docForPrimFns content =
    if null notListed
    then header ++ concatMap doc (primFns content)
    else panic $ "The following primitive functions need to be added to the reference doc: " <> T.intercalate ", " notListed
  where
    header :: [Block]
    header = [ Header 2 nullAttr (parseInlineMarkdown "Primitive functions")
             , Para $ parseInlineMarkdown $ primFnsDoc content
             ]

    notListed :: [Text]
    notListed = Map.keys primFnsMap \\ primFns content

    doc :: Text -> [Block]
    doc primName = case Map.lookup primName primFnsMap of
        Nothing -> panic $ "Unknown primitive function " <> primName
        Just (Doc.Docd Nothing _) -> panic $ "Missing documentation for primitive function " <> primName
        Just (Doc.Docd (Just docString) _) -> valueDoc primName docString (PrimFn $ Ident primName)

    primFnsMap :: Map Text (Doc.Docd ([Value] -> Lang IO Value))
    primFnsMap = Map.mapKeys fromIdent $ getPrimFns $ bindingsPrimFns (replBindings [])

parseInlineMarkdown :: Text -> [Inline]
parseInlineMarkdown t = case parseMarkdownBlocks t of
  [Para is] -> is
  _         -> panic $ "Expecting inline markdown but got blocks: " <> t

parseMarkdownBlocks :: Text -> [Block]
parseMarkdownBlocks t = case runPure (readCommonMark Default.def t) of
  Right (Pandoc _ block) -> block
  Left err               -> panic $ "Invalid markdown: " <> show err
