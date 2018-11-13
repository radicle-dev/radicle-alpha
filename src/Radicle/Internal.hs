{-# LANGUAGE TemplateHaskell #-}
module Radicle.Internal
    ( module X
    , mkIdent
    , ident
    , kword
    ) where

import           Prelude (String)
import           Protolude

import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as Syntax
import           Radicle.Internal.Annotation as X
import           Radicle.Internal.CLI as X
import           Radicle.Internal.Core as X
import           Radicle.Internal.Effects as X
import           Radicle.Internal.Identifier as X
import           Radicle.Internal.Interpret as X
import           Radicle.Internal.Parse as X
import           Radicle.Internal.Pretty as X
import           Radicle.Internal.PrimFns as X

import qualified Text.Megaparsec as M

-- | Smart constructor for Ident.
mkIdent :: Text -> Maybe Ident
mkIdent t = case runIdentity (M.runParserT valueP "" t) of
    Right (Atom i) -> pure i
    _              -> Nothing

expQuot :: Text -> (String -> Syntax.Q Syntax.Exp) -> QuasiQuoter
expQuot name e = QuasiQuoter
    { quoteExp = e
    , quoteType = panic err
    , quotePat = panic err
    , quoteDec = panic err
    }
  where
    err = name <> " only works for expressions"

-- | A quasiquoter that checks that an identifier is valid at compile-time.
ident :: QuasiQuoter
ident =
  expQuot "ident" $ \s -> [| case mkIdent s of
    Nothing -> panic $ "Not a valid identifier: " <> s
    Just i  -> i |]

kword :: QuasiQuoter
kword =
  expQuot "kword" $ \s -> [| case mkIdent s of
    Nothing -> panic $ "Not a valid keyword: " <> s
    Just i  -> Keyword i |]
