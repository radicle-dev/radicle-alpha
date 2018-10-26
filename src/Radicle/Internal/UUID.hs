{-# LANGUAGE UndecidableInstances #-}

module Radicle.Internal.UUID where

import           Protolude

import           Data.UUID
import           Data.UUID.V4

class Monad m => MonadUUID m where
  uuid :: m Text

instance MonadUUID IO where
  uuid = toText <$> nextRandom

isUUID :: Text -> Bool
isUUID t = isJust $ fromText t
