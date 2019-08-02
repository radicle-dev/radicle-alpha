module Radicle.Internal.Json where

import           Protolude

import           Control.Monad.Fail
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Sci
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
--
-- >>> import qualified Data.Vector as Vector
-- >>> renderCompactPretty <$> A.parseMaybe jsonToValue (A.String "{:foo 42}")
-- Just "{:foo 42}"
jsonToValue :: A.Value -> A.Parser Value
jsonToValue = A.withText "Value" $ \t -> do
    case parse "[daemon]" t of
      Left err -> fail $ "failed to parse Radicle expression: " <> show err
      Right v  -> pure v

-- | Pretty-prints a Radicle value, and wraps it in a JSON string.
--
-- >>> import Data.Aeson (encode)
-- >>> import Data.Sequence (fromList)
-- >>> encode $ valueToJson (Vec (fromList [Number 1, String "foo"]))
-- "\"[1\\n\\\"foo\\\"]\""
valueToJson :: Value -> A.Value
valueToJson = A.String . renderCompactPretty

-- | Convert a radicle `Value` into an 'aeson' value, if possible.
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ List [Number 3, String "hi"]
-- "{\"Right\":[3,\"hi\"]}"
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(String "foo", String "bar")]
-- "{\"Right\":{\"foo\":\"bar\"}}"
--
-- This fails for lambdas, since lambdas capture an entire environment (possibly
-- recursively). It will convert dicts as long as it can stringify all the keys,
-- that is, if they are strings, keywords, atoms, numbers of booleans.
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(Number 3, String "bar")]
-- "{\"Right\":{\"3\":\"bar\"}}"
--
-- >>> import Data.Aeson (encode)
-- >>> encode $ maybeJson $ Dict $ Map.fromList [(List [Number 3], String "bar")]
-- "{\"Left\":\"Cannot convert values of type List to a JSON key\"}"
maybeJson :: Value -> Either Text A.Value
maybeJson = \case
    Number n -> A.Number <$> isSci n
    String s -> pure $ A.String s
    Keyword i -> pure $ A.String (showIdent i)
    Atom i -> pure $ A.String (showIdent i)
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
    jsonKey (String s)   = pure s
    jsonKey (Keyword i)  = pure (showIdent i)
    jsonKey (Atom i)     = pure (showIdent i)
    jsonKey n@(Number _) = pure (renderCompactPretty n)
    jsonKey (Boolean b)  = pure $ if b then "true" else "false"
    jsonKey v            = Left $ cantConvertType v "a JSON key"

    cantConvertType v toThis = "Cannot convert values of type " <> toS (Type.typeString (valType v)) <> " to " <> toThis


-- | Converts an `aeson` value to a radicle one.
fromJson :: A.Value -> Value
fromJson = \case
  -- Note that using `toRational` would make this function unsafe when reading
  -- user input.
  A.Number n -> Number (toRational (Sci.toRealFloat n :: Double))
  A.String s -> String s
  A.Bool b -> Boolean b
  A.Array xs -> Vec $ Seq.fromList (JsonVector.toList (fromJson <$> xs))
  A.Object kvs -> Dict $ Map.fromList $ bimap String fromJson <$> HashMap.toList kvs
  A.Null -> Keyword (NakedT "nothing")
