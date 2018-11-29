module Radicle.Internal.Type where

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
  | TDict
  | TStructure
  | TRef
  | THandle
  | TProcHandle
  deriving (Eq, Show, Read, Generic)

instance Serialise Type
