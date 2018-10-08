module Radicle.Internal.Hash where

import           Protolude hiding (hash)

import           Crypto.Hash
import           Data.Copointed (Copointed(..))
import           Foreign.Storable

import           Radicle.Internal.Annotation
import           Radicle.Internal.Core
import           Radicle.Internal.Pretty

blake2b_256 :: ByteString -> Digest Blake2b_256
blake2b_256 = hash

hashText :: Text -> Text
hashText = show . blake2b_256 . toS

hashRad :: (Copointed t, Annotation t) => Annotated t ValueF -> Text
hashRad = hashText . renderCompactPretty

-- Merkle trees

class HasHash a where
  hashed :: a -> BS

type BS = ByteString

combi :: [BS] -> BS
combi = show . blake2b_256 . mconcat

data MerkleTree
  = Empty
  | NonEmpty Word32 Tree
  deriving (Show)

merkleTreeHash :: MerkleTree -> Maybe BS
merkleTreeHash = \case
  Empty -> Nothing
  NonEmpty _ t -> pure $ hashed t

data Tree
  = TBranch Branch
  | TLeaf Leaf
  deriving (Show)

data Leaf = Leaf BS
  deriving (Show)

data Branch = Branch
  { here  :: BS
  , left  :: Tree
  , right :: Maybe Tree
  } deriving (Show)

instance HasHash Tree where
  hashed (TLeaf (Leaf h)) = h
  hashed (TBranch b)      = here b

instance HasHash (Maybe Tree) where
  hashed Nothing  = "right-empty"
  hashed (Just t) = hashed t

hashLeaf :: BS -> BS
hashLeaf x = combi ["leaf", x]

mkLeaf :: BS -> Tree
mkLeaf = TLeaf . Leaf . hashLeaf

hashBranch :: BS -> BS -> BS
hashBranch x y = combi ["branch", x, y]

mkBranch :: Tree -> Maybe Tree -> Tree
mkBranch l r_ = TBranch Branch
  { here = hashBranch (hashed l) (hashed r_)
  , left = l
  , right = r_
  }

mkMerkleTree :: [BS] -> MerkleTree
mkMerkleTree [] = Empty
mkMerkleTree xs' = NonEmpty (fromIntegral (length xs')) $ keepPairing (mkLeaf <$> xs')
  where
    pair = \case
      []       -> []
      (x:y:xs) -> mkBranch x (Just y) : pair xs
      [x]      -> [mkBranch x Nothing]

    keepPairing = \case
      []  -> panic "impossible"
      [t] -> t
      ts  -> keepPairing (pair ts)

-- Proofs

data Side = LeftSide | RightSide deriving (Eq, Show)

data ProofElem = ProofElem
  { sibling :: BS
  , side    :: Side
  } deriving (Show)

type Proof = [ProofElem]

-- | Given an (starting at 0) index 'i' of a sequence of length 'n', returns the
-- sequence of sides for the proof, from the leaf to the root.
indexSides :: Word32 -> Word32 -> [Side]
indexSides n i = take b $ boolToSide <$> bitList i
  where
    b = ceiling $ logBase 2 (fromIntegral n :: Double)
    bitList x = map (testBit x) [0..8*(sizeOf x)-1]
    boolToSide = \case
      True -> RightSide
      False -> LeftSide

-- | Given an index 'i' and a merkle-tree, returns the proof for the value of the
-- 'i'-th element.
mkProof :: Word32 -> MerkleTree -> Maybe Proof
mkProof _ Empty = Nothing
mkProof i (NonEmpty n _) | i >= n = Nothing
mkProof i (NonEmpty n t) =
  let sides = reverse (indexSides n i) in
  getProof sides t
  where
    getProof (LeftSide:sides) (TBranch b) = do
      rest <- getProof sides (left b)
      pure $ ProofElem {sibling = hashed (right b), side = LeftSide } : rest
    getProof (RightSide:sides) (TBranch b) = do
      r <- right b
      rest <- getProof sides r
      pure $ ProofElem {sibling = hashed (left b), side = RightSide} : rest
    getProof [] (TLeaf _) = pure []
    getProof _ _ = Nothing

-- | Checks a proof 'pf' that element at index 'i' in sequence of total length
-- 'n' is indeed 'el'.
checkProof
  :: Word32 -- ^ Sequence size
  -> Word32 -- ^ Index
  -> BS     -- ^ Root hash
  -> BS     -- ^ Element
  -> Proof  -- ^ Proof
  -> Bool
checkProof n i root el pf =
     i < n
  && reverse (indexSides n i) == (side <$> pf)
  && zipProof == root
  where
    zipProof = foldr applyPfElem (hashLeaf el) pf

    applyPfElem p x = case side p of
      LeftSide  -> hashBranch x (sibling p)
      RightSide -> hashBranch (sibling p) x
