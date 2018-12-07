{-# OPTIONS_GHC -fno-warn-orphans #-}

module Radicle.Internal.Pretty where

import           Protolude hiding (Type, TypeError, (<>))

import           Data.Copointed (Copointed(..))
import qualified Data.Map as Map
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal (Color(..), color)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Term
import           Data.Text.Prettyprint.Doc.Render.Text
import           Text.Megaparsec.Error (parseErrorPretty)
import           Text.Megaparsec.Pos (sourcePosPretty)

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Effects.Capabilities (Stdout(..))
import           Radicle.Internal.Identifier (Ident(..))
import           Radicle.Internal.Type

class PrettyV a where
    prettyV :: a -> Doc Type

-- | We can't just pretty print the pointer id since that would break
-- referential transparency, so instead we just label refs as '<ref>'
instance PrettyV Reference where
    prettyV _ = annotate TRef $ angles "ref"

-- | As with references, we do not print the actual counter of the handle.
instance Pretty Hdl where
    pretty _ = angles "handle"

-- | As with references, we do not print the actual counter of the handle.
instance Pretty ProcHdl where
    pretty _ = angles "prochandle"

instance Pretty Ident where
    pretty (Ident i) = pretty i

instance PrettyV Type where
    prettyV = prettyV . typeToValue

instance forall t. (Copointed t, Ann.Annotation t) => PrettyV (Ann.Annotated t ValueF) where
    prettyV v = annotate (valType v) $ case v of
        Atom i -> pretty i
        Keyword i -> ":" <> pretty i
        Ref i -> prettyV i
        Handle i -> pretty i
        ProcHandle i -> pretty i
        String t -> "\"" <> pretty (escapeStr t) <> "\""
        Number (a :% b) -> pretty a <> if b == 1 then "" else "/" <> pretty b
        Boolean True -> "#t"
        Boolean False -> "#f"
        List vs -> case vs of
          []   -> "()"
          [v'] -> parens $ prettyV v'
          _    -> parens $ hang 1 (sep $ prettyV <$> vs)
        Vec vs -> case vs of
          Empty        -> "[]"
          v' :<| Empty -> brackets $ prettyV v'
          _            -> brackets . sep $ prettyV <$> toList vs
        PrimFn i -> pretty i
        Dict mp -> braces . align $
            sep [ prettyV k <+> prettyV val
                | (k, val) <- Map.toList mp ]
        Lambda ids vals _ ->
          let args :: Ann.Annotated t ValueF = Vec $ Seq.fromList (Atom <$> ids)
          in parens $
               annotate TAtom "fn" <+>
               align (sep [ prettyV args
                          , sep $ prettyV <$> toList vals
                          ])
      where
        -- We print string literals escaped just like Haskell does.
        escapeStr = T.init . T.tail . show

instance Pretty Ann.SrcPos where
    pretty (Ann.SrcPos pos)        = pretty (sourcePosPretty pos)
    pretty (Ann.InternalPos stack) = pretty stack

instance PrettyV r => PrettyV (LangError r) where
    prettyV (LangError stack err) = vsep $
        [prettyV err, indent 2 $ vsep $ map pretty (reverse stack)]

instance PrettyV r => PrettyV (LangErrorData r) where
    prettyV v = case v of
        UnknownIdentifier i -> "Unknown identifier:" <+> pretty i
        Impossible t -> "This cannot be!" <+> pretty t
        TypeError fname i t val -> vsep
          [ "Type error:" <+> pretty fname <+> "expects a value of type"
            <+> prettyV t <+> "in the" <+> pretty (pos (i + 1)) <+> "argument."
          , "But got a" <+> prettyV (valType val) <> ":"
          , indent 2 $ prettyV val ]
        NonFunctionCalled val -> vsep
          [ "Value was invoked as a function, but it has type" <+> prettyV (valType val) <> ":"
          , indent 2 $ prettyV val ]
        WrongNumberOfArgs t x y -> "Wrong number of args in" <+> pretty t
                                 <+> "Expected:" <+> pretty x
                                 <+> "Got:" <+> pretty y
        NonHashableKey -> "Non-hashable key in dict."
        ModuleError me -> case me of
          MissingDeclaration -> "Modules must start with a metadata declaration"
          InvalidDeclaration t decl -> vsep
            [ "Invalid module declaration:" <+> pretty t <> ":"
            , indent 2 $ prettyV decl
            ]
          UndefinedExports n is ->
            "Module" <+> prettyV (asValue (Atom n))
            <+> "has exports which are not defined:"
            <+> sep (prettyV . asValue . Atom <$> is)
        OtherError t -> "Error:" <+> pretty t
        SpecialForm f t -> "Error using special form" <+> pretty f <> ":" <+> pretty t <> "."
        ParseError t -> "Parser error:" <+> pretty (parseErrorPretty t)
        Exit -> "Exit"
        ThrownError i val -> "Exception" <+> pretty i <+> prettyV val
        PatternMatchError e -> case e of
          NoMatch -> "Pattern match(es) are non-exhaustive."
          NoValue -> "The `match` special form must be given a value to match on."
          BadBindings p -> "Faulty pattern function. Pattern functions must return\
                           \ `[:just b]` where `b` is a dict of new bindings (from\
                           \ atoms to values), or `:nothing`:" <+> prettyV p
      where
        pos :: Int -> Text
        pos 1 = "1st"
        pos 2 = "2nd"
        pos 3 = "3rd"
        pos n = show n <> "th"


-- | A fast and compact layout. Primarily intended for testing.
--
-- Examples:
--
-- >>> renderCompactPretty (asValue (List [String "hi", String "there"]))
-- "(\"hi\"\n\"there\")"
renderCompactPretty :: PrettyV v => v -> Text
renderCompactPretty = renderStrict . layoutCompact . prettyV

-- | Render prettily into text, with specified width.
--
-- Examples:
--
-- >>> renderPretty Unbounded (asValue (List [String "hi", String "there"]))
-- "(\"hi\" \"there\")"
--
-- >>> renderPretty (AvailablePerLine 6 0.5) (asValue (List [String "hi", String "there"]))
-- "(\"hi\"\n  \"there\")"
renderPretty :: PrettyV v => PageWidth -> v -> Text
renderPretty pg = renderStrict . layoutSmart (LayoutOptions pg) . prettyV

-- | 'renderPretty', but with default layout options (80 chars, 1.0 ribbon)
--
-- Examples:
--
-- >>> renderPrettyDef (asValue (List [String "hi", String "there"]))
-- "(\"hi\" \"there\")"
renderPrettyDef :: PrettyV v => v -> Text
renderPrettyDef = renderStrict . layoutSmart defaultLayoutOptions . prettyV

-- | Pretty print the value with colors if @m@ supports it.
putPrettyAnsi :: (PrettyV v, Stdout m) => v -> m ()
putPrettyAnsi value = do
    supportsANSI' <- supportsANSI
    let docStream = layoutSmart defaultLayoutOptions $ prettyV value
    if supportsANSI'
    then
        putStrS $ Term.renderStrict $ reAnnotateS toAnsi docStream
    else
        putStrS $ renderStrict docStream

toAnsi :: Type -> Term.AnsiStyle
toAnsi = \case
    TKeyword -> color Magenta
    TString -> color Green
    TNumber -> color Yellow
    TBoolean -> color Cyan
    TRef -> color Red
    _ -> mempty
