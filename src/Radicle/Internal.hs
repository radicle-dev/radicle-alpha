module Radicle.Internal
    ( module X
    , makeIdent
    , parse
    , parseTest
    ) where

import           Radicle.Internal.Chain as X
import           Radicle.Internal.Core as X
import           Radicle.Internal.Parse as X
import           Radicle.Internal.Pretty as X
import           Radicle.Internal.Repl as X

import           Control.Monad.Except
import           Data.Text (Text)
import qualified Text.Megaparsec as M

-- | Smart constructor for Ident.
makeIdent :: Text -> Maybe Ident
makeIdent = M.parseMaybe identP

-- | Parse a value, using the String as source name, and calling 'fail' with a
-- pretty error in case of a parse error.
parse :: MonadError String m => String -> Text -> m Value
parse file src = case M.parse valueP file src of
    Left err -> throwError $ M.parseErrorPretty' src err
    Right v  -> pure v

-- | Like 'parse', but uses "(test)" as the source name
parseTest :: MonadError String m => Text -> m Value
parseTest = parse "(test)"
