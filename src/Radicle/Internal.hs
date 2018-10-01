{-# LANGUAGE TemplateHaskell #-}
module Radicle.Internal
    ( module X
    , mkIdent
    , ident
    ) where

import           Prelude (error)
import           Protolude

import           Language.Haskell.TH.Quote
import           Radicle.Internal.CLI as X
import           Radicle.Internal.Core as X
import           Radicle.Internal.Effects as X
import           Radicle.Internal.Interpret as X
import           Radicle.Internal.Parse as X
import           Radicle.Internal.Pretty as X
import           Radicle.Internal.Primops as X

import qualified Text.Megaparsec as M

-- | Smart constructor for Ident.
mkIdent :: Text -> Maybe Ident
mkIdent t = case runReader (M.runParserT identP "" t) [] of
    Left _  -> Nothing
    Right v -> pure v

-- | A quasiquoter that checks that an identifier is valid at compile-time.
ident :: QuasiQuoter
ident = QuasiQuoter
    { quoteExp = \s -> [| case mkIdent s of
        Nothing -> error $ "Not a valid identifier: " <> s
        Just i  -> i |]
    , quoteType = error "ident only works for expressions"
    , quotePat = error "ident only works for expressions"
    , quoteDec = error "ident only works for expressions"
    }
