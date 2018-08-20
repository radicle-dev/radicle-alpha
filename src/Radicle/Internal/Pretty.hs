{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Pretty where

import           Protolude hiding (TypeError, (<>))

import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Text.Megaparsec.Error (parseErrorPretty)

import           Radicle.Internal.Core

-- | We can't just pretty print the pointer id since that would break
-- referential transparency, so instead we just label refs as '<ref>'
instance Pretty Reference where
    pretty _ = angles "ref"

instance Pretty Ident where
    pretty (Ident i) = pretty i

instance Pretty Value where
    pretty v = case v of
        Atom i -> pretty i
        Keyword i -> ":" <> pretty i
        Ref i -> parens $ "ref" <+> pretty i
        String t -> "\"" <> pretty (escapeStr t) <> "\""
        Number n -> pretty (show n :: Text)
        Boolean True -> "#t"
        Boolean False -> "#f"
        List vs -> parens (sep (pretty <$> vs))
        Primop i -> pretty i
        Dict mp -> parens $
            "dict" <+> sep [ pretty k <+> pretty val
                           | (k, val) <- Map.toList mp ]
        Lambda ids vals _ -> parens $
            "lambda" <+> parens (sep $ pretty <$> ids)
                     <+> sep (pretty <$> toList vals)
      where
        escapeStr = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

instance Pretty r => Pretty (LangError r) where
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
        ThrownError i val -> "Exception" <+> pretty i <+> pretty val


-- | A fast and compact layout. Primarily intended for testing.
--
-- Examples:
--
-- >>> renderCompactPretty (List [String "hi", String "there"])
-- "(\"hi\"\n\"there\")"
renderCompactPretty :: Pretty v => v -> Text
renderCompactPretty = renderStrict . layoutCompact . pretty

-- | Render prettily into text, with specified width.
--
-- Examples:
--
-- >>> import Data.Void
-- >>> renderPretty Unbounded (List [String "hi", String "there"])
-- "(\"hi\" \"there\")"
--
-- >>> renderPretty (AvailablePerLine 6 0.5) (List [String "hi", String "there"])
-- "(\"hi\"\n\"there\")"
renderPretty :: Pretty v => PageWidth -> v -> Text
renderPretty pg = renderStrict . layoutSmart (LayoutOptions pg) . pretty

-- | 'renderPretty', but with default layout options (80 chars, 1.0 ribbon)
--
-- Examples:
--
-- >>> import Data.Void
-- >>> renderPrettyDef (List [String "hi", String "there"])
-- "(\"hi\" \"there\")"
renderPrettyDef :: Pretty v => v -> Text
renderPrettyDef = renderStrict . layoutSmart defaultLayoutOptions . pretty
