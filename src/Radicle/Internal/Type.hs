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
  deriving (Eq, Show, Read, Generic)

instance Serialise Type

subtype :: Type -> Type -> Bool
subtype x y             | x == y = True
subtype TList TSequence = True
subtype TVec  TSequence = True
subtype _ _             = False
