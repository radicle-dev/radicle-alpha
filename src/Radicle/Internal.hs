module Radicle.Internal
    ( module X
    , makeIdent
    ) where

import           Radicle.Internal.Core as X
import           Radicle.Internal.Parse as X
import           Radicle.Internal.Pretty as X
import           Radicle.Internal.Subscriber as X

import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Text.Megaparsec as M

-- | Smart constructor for Ident.
makeIdent :: Text -> Maybe Ident
makeIdent t = case runReader (M.runParserT identP "" t) [] of
    Left _ -> Nothing
    Right v -> pure v
