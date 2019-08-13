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
                <> "(load-ns! \"prelude/test-macro.rad\")"
                <> foldMap (\m -> "(load-ns! \"" <> m <> ".rad\")") (modules content)
                <> "(get-current-state))"
               )
               bindings
    let res = res_ `lPanic` "Error running the prelude."
    let bnds = bindingsFromRadicle res `lPanic` "Couldn't convert radicle state."
    let nss = bindingsNamespaces bnds
    let rst = runPure (writeRST Default.def $ Pandoc nullMeta (doc content nss)) `lPanic` "Couldn't generate RST"
    writeFile "docs/source/reference.rst" rst
    writeFile "docs/source/daemon-api.yaml" (toS (encode Daemon.swagger))
  where

    doc :: Content -> Namespaces -> [Block]
    doc content nss =
      [ Header 1 nullAttr (parseInlineMarkdown "Radicle Reference")
      , Para $ parseInlineMarkdown (intro content)
      ] ++ docForPrimFns content ++
      [ Header 2 nullAttr (parseInlineMarkdown "Prelude modules")
      , Para $ parseInlineMarkdown $ preludeModulesDoc content
      ] ++ foldMap (nsDoc nss) (modules content)

nsDoc :: Namespaces -> Text -> [Block]
nsDoc nss name =
    case Map.lookup (Naked name) nss of
        Nothing -> panic $ "Couldn’t find namespace " <> name
        Just (Doc.Docd Nothing _) -> panic $ "Namespace " <> name <> " is not documented"
        Just (Doc.Docd (Just docString) ns) ->
             header docString
          <> concatMap (exportsDoc name ns) (getExports ns)
  where
    header :: Text -> [Block]
    header docString =
        [ Header 2 nullAttr [ Code nullAttr (toS name) ]
        , Para (parseInlineMarkdown docString) ]

    getExports :: Namespace -> [Text]
    getExports ns = [showUnnamespaced k | (k, Here Public _) <- Map.toList (bindings ns) ]

exportsDoc :: Text -> Namespace -> Text -> [Block]
exportsDoc nsName ns name =
    case Map.lookup (NakedU (Naked name)) (bindings ns) of
        Nothing -> panic $ "Unknown export " <> name <> "in namespace " <> nsName
        Just (Here _ (Doc.Docd Nothing _)) -> panic $ "Missing documentation for " <> name <> " in " <> nsName
        Just (Here Public (Doc.Docd (Just docString) value)) -> valueDoc name docString value
        Just (Here Private _) -> panic $ "Def was not public: " <> name <> "in namespace " <> nsName
        Just (There _ _) -> panic $ "Export was to be found in another namespace " <> name <> "in namespace " <> nsName

lPanic :: Show a => Either a p -> Text -> p
lPanic (Left e)  m = panic $ m <> ": " <> show e
lPanic (Right r) _ = r

valueDoc :: Text -> Text -> Value -> [Block]
valueDoc name docString value =
  [ Header 3 nullAttr [Code nullAttr (toS title) ] ]
  <> parseMarkdownBlocks docString
  where
    title = fromMaybe name $ callExample (NakedU (Naked name)) value

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
        Just (Doc.Docd (Just docString) _) -> valueDoc primName docString (PrimFn $ Naked primName)

    primFnsMap :: Map Text (Doc.Docd ([Value] -> Lang IO Value))
    primFnsMap = Map.mapKeys fromNaked $ getPrimFns $ bindingsPrimFns (replBindings [])

parseInlineMarkdown :: Text -> [Inline]
parseInlineMarkdown t = case parseMarkdownBlocks t of
  [Para is] -> is
  _         -> panic $ "Expecting inline markdown but got blocks: " <> t

parseMarkdownBlocks :: Text -> [Block]
parseMarkdownBlocks t = case runPure (readCommonMark Default.def t) of
  Right (Pandoc _ block) -> block
  Left err               -> panic $ "Invalid markdown: " <> show err
