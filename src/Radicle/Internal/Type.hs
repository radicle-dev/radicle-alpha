module Radicle.Internal.Type where

import           Prelude (String)
import           Protolude hiding (Type)

import           Codec.Serialise (Serialise)

data Type
  = TAtom
  | TKeyword
  | TString
  | TNumber
  | TBoolean
  | TList
  | TVec
  | TSequence
  | TFunction
  | TMacro
  | TDict
  | TStructure
  | TRef
  | THandle
  | TProcHandle
  | TEnv
  | TState
  deriving (Eq, Show, Read, Generic)

instance Serialise Type

typeString :: Type -> String
typeString = drop 1 . show
