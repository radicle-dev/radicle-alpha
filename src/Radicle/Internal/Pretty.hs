{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Pretty where

import           Data.List.NonEmpty (toList)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Text.Megaparsec.Error (parseErrorPretty)

import           Radicle.Internal.Core

instance Pretty Ident where
    pretty (Ident i) = pretty i

instance Pretty Value where
    pretty v = case v of
        Atom i -> pretty i
        Ref i -> parens $ "ref" <+> pretty i
        String t -> "\"" <> pretty (escapeStr t) <> "\""
        Number n -> pretty $ show n
        Boolean True -> "#t"
        Boolean False -> "#f"
        List vs ->  parens $ sep ("list" : (pretty <$> vs))
        Primop i -> pretty i
        Apply fn vs -> parens $ pretty fn <+> sep (pretty <$> vs)
        SortedMap mp -> parens $
            "sorted-map" <+> sep [ pretty k <+> pretty val
                                 | (k, val) <- Map.toList mp ]
        Lambda ids vals _ -> parens $
            "lambda" <+> parens (sep $ pretty <$> ids)
                     <+> sep (pretty <$> toList vals)
      where
        escapeStr = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

instance Pretty LangError where
    pretty v = case v of
        UnknownIdentifier i -> "Unknown identifier:" <+> pretty i
        Impossible t -> "This cannot be!" <+> pretty t
        TypeError t -> "Type error:" <+> pretty t
        WrongNumberOfArgs t x y -> "Wrong number of args in" <+> pretty t
                                 <+> "Expected:" <+> pretty x
                                 <+> "Got:" <+> pretty y
        OtherError t -> "Error:" <+> pretty t
        ParseError t -> "Parser error:" <+> pretty (parseErrorPretty t)
        Exit -> "Exit"


-- | A fast and compact layout. Primarily intended for testing.
--
-- Examples:
--
-- >>> renderCompactPretty (List [String "hi", String "there"])
-- "(list\n\"hi\"\n\"there\")"
renderCompactPretty :: Pretty v => v -> Text
renderCompactPretty = renderStrict . layoutCompact . pretty

-- | Render prettily into text, with specified width.
--
-- Examples:
--
-- >>> renderPretty Unbounded (List [String "hi", String "there"])
-- "(list \"hi\" \"there\")"
--
-- >>> renderPretty (AvailablePerLine 6 0.5) (List [String "hi", String "there"])
-- "(list\n\"hi\"\n\"there\")"
renderPretty :: Pretty v => PageWidth -> v -> Text
renderPretty pg = renderStrict . layoutSmart (LayoutOptions pg) . pretty

-- | 'renderPretty', but with default layout options (80 chars, 1.0 ribbon)
--
-- Examples:
--
-- >>> renderPrettyDef (List [String "hi", String "there"])
-- "(list \"hi\" \"there\")"
renderPrettyDef :: Pretty v => v -> Text
renderPrettyDef = renderStrict . layoutSmart defaultLayoutOptions . pretty
