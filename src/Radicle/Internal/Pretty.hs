{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Radicle.Internal.Pretty where

import           Protolude hiding (Type, TypeError, (<>))

import           Data.Copointed (Copointed(..))
import qualified Data.Map as Map
import           Data.Sequence (Seq(..))
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           GHC.Exts (IsString(..))
import           Text.Megaparsec.Error (parseErrorPretty)
import           Text.Megaparsec.Pos (sourcePosPretty)

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Type

-- | We can't just pretty print the pointer id since that would break
-- referential transparency, so instead we just label refs as '<ref>'
instance Pretty Reference where
    pretty _ = angles "ref"

instance Pretty Ident where
    pretty (Ident i) = pretty i

instance Pretty Type where
  pretty = fromString . drop 1 . show

instance (Copointed t, Ann.Annotation t) => Pretty (Ann.Annotated t ValueF) where
    pretty v = case v of
        Atom i -> pretty i
        Keyword i -> ":" <> pretty i
        Ref i -> pretty i
        String t -> "\"" <> pretty (escapeStr t) <> "\""
        Number (a :% b) -> pretty a <> if b == 1 then "" else "/" <> pretty b
        Boolean True -> "#t"
        Boolean False -> "#f"
        List vs -> case vs of
          []   -> "()"
          [v'] -> parens $ pretty v'
          _    -> parens $ hang 1 (sep $ pretty <$> vs)
        Vec vs -> case vs of
          Empty        -> "[]"
          v' :<| Empty -> brackets $ pretty v'
          _            -> brackets . sep $ pretty <$> toList vs
        PrimFn i -> pretty i
        Dict mp -> braces . align $
            sep [ pretty k <+> pretty val
                | (k, val) <- Map.toList mp ]
        Lambda ids vals _ -> parens $
            "fn" <+> align (sep
                        [ brackets . sep $ pretty <$> ids
                        , sep $ pretty <$> toList vals
                        ])
      where
        -- We print string literals escaped just like Haskell does.
        escapeStr = T.init . T.tail . show

instance Pretty Ann.SrcPos where
    pretty (Ann.SrcPos pos)        = pretty (sourcePosPretty pos)
    pretty (Ann.InternalPos stack) = pretty stack

instance Pretty r => Pretty (LangError r) where
    pretty (LangError stack err) = vsep $
        [pretty err, "Call stack:"] ++ map pretty (reverse stack)

instance Pretty r => Pretty (LangErrorData r) where
    pretty v = case v of
        UnknownIdentifier i -> "Unknown identifier:" <+> pretty i
        Impossible t -> "This cannot be!" <+> pretty t
        TypeError fname pos t val -> vsep
          [ "Type error:" <+> pretty fname <+> "expects a value of type"
            <+> pretty t <+> "in the" <+> pretty pos <> "th argument."
          , "But got a" <+> pretty (valType val) <> ":"
          , indent 2 $ pretty val ]
        WrongNumberOfArgs t x y -> "Wrong number of args in" <+> pretty t
                                 <+> "Expected:" <+> pretty x
                                 <+> "Got:" <+> pretty y
        NonHashableKey -> "Non-hashable key in dict."
        OtherError t -> "Error:" <+> pretty t
        SpecialForm f t -> "Special form error: error when using" <+> pretty f <> ":" <+> pretty t
        ParseError t -> "Parser error:" <+> pretty (parseErrorPretty t)
        Exit -> "Exit"
        ThrownError i val -> "Exception" <+> pretty i <+> pretty val
        PatternMatchError e -> case e of
          NoMatch -> "Pattern match(es) are non-exhaustive."
          NoValue -> "The `match` special form must be given a value to match on."
          BadBindings p -> "Faulty pattern function. Pattern functions must return\
                           \ `[:just b]` where `b` is a dict of new bindings (from\
                           \ atoms to values), or `:nothing`:" <+> pretty p


-- | A fast and compact layout. Primarily intended for testing.
--
-- Examples:
--
-- >>> renderCompactPretty (asValue (List [String "hi", String "there"]))
-- "(\"hi\"\n\"there\")"
renderCompactPretty :: Pretty v => v -> Text
renderCompactPretty = renderStrict . layoutCompact . pretty

-- | Render prettily into text, with specified width.
--
-- Examples:
--
-- >>> renderPretty Unbounded (asValue (List [String "hi", String "there"]))
-- "(\"hi\" \"there\")"
--
-- >>> renderPretty (AvailablePerLine 6 0.5) (asValue (List [String "hi", String "there"]))
-- "(\"hi\"\n  \"there\")"
renderPretty :: Pretty v => PageWidth -> v -> Text
renderPretty pg = renderStrict . layoutSmart (LayoutOptions pg) . pretty

-- | 'renderPretty', but with default layout options (80 chars, 1.0 ribbon)
--
-- Examples:
--
-- >>> renderPrettyDef (asValue (List [String "hi", String "there"]))
-- "(\"hi\" \"there\")"
renderPrettyDef :: Pretty v => v -> Text
renderPrettyDef = renderStrict . layoutSmart defaultLayoutOptions . pretty
