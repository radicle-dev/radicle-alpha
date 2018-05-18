{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Pretty where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Radicle.Internal.Core

instance Pretty Ident where
    pretty (Ident i) = pretty i

instance Pretty Value where
    pretty v = case v of
        Atom i -> pretty i
        String t -> "\"" <> pretty (escapeStr t) <> "\""
        Boolean True -> "#t"
        Boolean False -> "#f"
        List vs -> "'" <> (parens . sep $ pretty <$> vs)
        Primop i -> pretty i
        Apply fn vs -> parens $ pretty fn <+> sep (pretty <$> vs)
        SortedMap mp -> parens $
            "sorted-map" <+> sep [ pretty k <+> pretty val
                                 | (k, val) <- Map.toList mp ]
        Lambda ids val _ -> parens $
            "lambda" <+> parens (sep $ pretty <$> ids)
                     <+> pretty val
      where
        escapeStr = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

-- | A fast and compact layout. Primarily intended for testing.
--
-- Examples:
--
-- >>> renderCompactPretty (List [String "hi", String "there"])
-- "'(\"hi\"\n\"there\")"
renderCompactPretty :: Value -> Text
renderCompactPretty = renderStrict . layoutCompact . pretty

-- | Render prettily into text, with specified width.
--
-- Examples:
--
-- >>> renderPretty Unbounded (List [String "hi", String "there"])
-- "'(\"hi\" \"there\")"
--
-- >>> renderPretty (AvailablePerLine 6 0.5) (List [String "hi", String "there"])
-- "'(\"hi\"\n\"there\")"
renderPretty :: PageWidth -> Value -> Text
renderPretty pg = renderStrict . layoutSmart (LayoutOptions pg) . pretty

-- | 'renderPretty', but with default layout options (80 chars, 1.0 ribbon)
--
-- Examples:
--
-- >>> renderPrettyDef (List [String "hi", String "there"])
-- "'(\"hi\" \"there\")"
renderPrettyDef :: Value -> Text
renderPrettyDef = renderStrict . layoutSmart defaultLayoutOptions . pretty
