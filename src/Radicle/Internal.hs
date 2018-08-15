module Radicle.Internal
    ( module X
    , mkIdent
    ) where

import           Protolude

import           Radicle.Internal.Core as X
import           Radicle.Internal.Parse as X
import           Radicle.Internal.Pretty as X
import           Radicle.Internal.Primops as X
import           Radicle.Internal.Subscriber as X
import           Radicle.Internal.CLI as X

import qualified Text.Megaparsec as M

-- | Smart constructor for Ident.
mkIdent :: Text -> Maybe Ident
mkIdent t = case runReader (M.runParserT identP "" t) [] of
    Left _  -> Nothing
    Right v -> pure v
