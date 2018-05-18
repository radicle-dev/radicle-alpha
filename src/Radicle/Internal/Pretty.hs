{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Pretty where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import Text.Megaparsec.Error

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

instance Pretty LangError where
    pretty v = case v of
        UnknownIdentifier i -> "unknown identifier " <+> pretty i
        Impossible i -> "the impossible happened! " <+> pretty i
        TypeError t -> "type error:" <+> pretty t
        WrongNumberOfArgs t expected actual ->
            "wrong number of args :" <+> pretty t <> nest 2 (
                    "expected" <+> pretty expected <>
                    line <>
                    "but got" <+> pretty actual)
        OtherError i -> "error: " <+> pretty i
        ParseError i ->
            "parse error:" <>
            line <>
            pretty (parseErrorPretty i)

-- | A fast and compact layout. Primarily intended for testing.
--
-- Examples:
--
-- >>> renderCompactPretty (List [String "hi", String "there"])
-- "'(\"hi\"\n\"there\")"
renderCompactPretty :: Pretty v => v -> Text
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
renderPretty :: Pretty v => PageWidth -> v -> Text
renderPretty pg = renderStrict . layoutSmart (LayoutOptions pg) . pretty

-- | 'renderPretty', but with default layout options (80 chars, 1.0 ribbon)
--
-- Examples:
--
-- >>> renderPrettyDef (List [String "hi", String "there"])
-- "'(\"hi\" \"there\")"
renderPrettyDef :: Pretty v => v -> Text
renderPrettyDef = renderStrict . layoutSmart defaultLayoutOptions . pretty
