{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Internal.Json where

import Protolude

import qualified Data.Aeson as A
import           Control.Monad.Fail

import Radicle.Internal.Core
import Radicle.Internal.Parse
import Radicle.Internal.Pretty

instance A.FromJSON Value where
  parseJSON = A.withText "Value" $ \t -> do
    case parse "[daemon]" t of
      Left err -> fail $ "failed to parse Radicle expression: " <> show err
      Right v  -> pure v

instance A.ToJSON Value where
  toJSON = A.String . renderCompactPretty
