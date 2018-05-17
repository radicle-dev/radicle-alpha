{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Pretty where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
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

compactPretty :: Value -> Text
compactPretty = renderStrict . layoutCompact . pretty
