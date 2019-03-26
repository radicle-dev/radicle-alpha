module Radicle.Internal.Json where

import           Protolude

import           Control.Monad.Fail
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as JsonVector

import           Radicle.Internal.Core
import           Radicle.Internal.Identifier
import           Radicle.Internal.Number
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import qualified Radicle.Internal.Type as Type

-- | Parses JSON into a Value. It expects the value to be encoded as a radicle
-- expression in a JSON string. Corresponds to the encoding given by
-- 'valueToJson'.
jsonToValue :: A.Value -> A.Parser Value
jsonToValue = A.withText "Value" $ \t -> do
    case parse "[daemon]" t of
      Left err -> fail $ "failed to parse Radicle expression: " <> show err
      Right v  -> pure v

-- | Encodes a radicle value as JSON. The value is encoded a radicle expression
-- formatted JSON string.
valueToJson :: Value -> A.Value
valueToJson = A.String . renderCompactPretty

-- | Convert a radicle `Value` into an 'aeson' value, if possible.
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ List [Number 3, String "hi"]
-- "[3,\"hi\"]"
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(String "foo", String "bar")]
-- "{\"foo\":\"bar\"}"
--
-- This fails for lambdas, since lambdas capture an entire environment (possibly
-- recursively). It also fails for dictionaries with non-stringy (string or
-- keyword) keys.
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(Number 3, String "bar")]
-- "null"
maybeJson :: Value -> Either Text A.Value
maybeJson = \case
    Number n -> A.Number <$> isSci n
    String s -> pure $ A.String s
    Keyword (Ident i) -> pure $ A.String i
    Boolean b -> pure $ A.Bool b
    List ls -> A.toJSON <$> traverse maybeJson ls
    Vec xs -> A.toJSON <$> traverse maybeJson (toList xs)
    Dict m -> do
      let kvs = Map.toList m
      ks <- traverse jsonKey (fst <$> kvs)
      vs <- traverse maybeJson (snd <$> kvs)
      pure $ A.Object (HashMap.fromList (zip ks vs))
    v -> Left $ cantConvertType v "JSON"
  where
    jsonKey :: Value -> Either Text Text
    jsonKey (String s)          = pure s
    jsonKey (Keyword (Ident i)) = pure i
    jsonKey n@(Number _)        = pure (renderCompactPretty n)
    jsonKey (Boolean b)         = pure $ if b then "true" else "false"
    jsonKey v                   = Left $ cantConvertType v "a JSON key"

    cantConvertType :: Value -> Text -> Text
    cantConvertType v toThis = "Can not convert values of type " <> toS (Type.typeString (valType v)) <> " to " <> toThis


-- | Converts an `aeson` value to a radicle one.
fromJson :: A.Value -> Value
fromJson = \case
  A.Number n -> Number (toRational n) -- TODO(james): think about untrusted sources.
  A.String s -> String s
  A.Bool b -> Boolean b
  A.Array xs -> Vec $ Seq.fromList (JsonVector.toList (fromJson <$> xs))
  A.Object kvs -> Dict $ Map.fromList $ bimap String fromJson <$> HashMap.toList kvs
  A.Null -> Keyword (Ident "nothing")
