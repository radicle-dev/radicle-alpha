{-# LANGUAGE TemplateHaskell #-}
module Radicle.TH
    ( ident
    , kword
    ) where

import           Prelude (String)
import           Protolude

import qualified Data.Text as T
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as Syntax

import           Radicle.Internal.Core
import           Radicle.Internal.Identifier
import           Radicle.Internal.Parse (mkIdent)

expQuot :: Text -> (String -> Syntax.Q Syntax.Exp) -> QuasiQuoter
expQuot name e = QuasiQuoter
    { quoteExp = e
    , quoteType = panic err
    , quotePat = panic err
    , quoteDec = panic err
    }
  where
    err = name <> " only works for expressions"

-- | Checks wheter an identifier is valid at compile-time.
-- @
--     [ident|foo] :: Ident
-- @
ident :: QuasiQuoter
ident =
  expQuot "ident" $ \s -> case mkIdent (toS s) of
    Nothing -> panic $ "Not a valid identifier: " <> toS s
    Just _  -> [| Ident (T.pack s) |]

-- | Produces a keyword 'Value'. Checks if the template is a valid
-- keyword at compile-time. The template must not include the leading
-- colon.
-- @
--     [kword|foo] :: Value
-- @
kword :: QuasiQuoter
kword =
  expQuot "kword" $ \s -> case mkIdent (toS s) of
    Nothing -> panic $ "Not a valid keyword: " <> toS s
    Just _  -> [| Keyword (Ident (T.pack s)) |]
