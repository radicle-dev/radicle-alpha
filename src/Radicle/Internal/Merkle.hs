module Radicle.Internal.Merkle where

import           Protolude hiding (hash)

import           Data.List (findIndex)
import           Foreign.Storable

import qualified Radicle.Internal.Annotation as Ann
import           Radicle.Internal.Core
import           Radicle.Internal.Hash

combi :: [ByteString] -> ByteString
combi = hash . mconcat

class HasHash a where
  hashed :: a -> ByteString

-- | A Merkle hash tree containing values 'a' at the leaves.
data MerkleTree a
  = Empty
  | NonEmpty Int (Tree a)
  deriving (Generic, Show)

instance FromRad Ann.WithPos a => FromRad Ann.WithPos (MerkleTree a)
instance ToRad   Ann.WithPos a => ToRad   Ann.WithPos (MerkleTree a)

hashMerkleTree :: Int -> ByteString -> ByteString
hashMerkleTree n x = combi ["non-empty-tree", show n, x]

instance HasHash (MerkleTree a) where
  hashed Empty          = "empty-tree"
  hashed (NonEmpty n t) = hashMerkleTree n (hashed t)

data Tree a
  = TBranch (Branch a)
  | TLeaf (Leaf a)
  deriving (Generic, Show)

instance FromRad Ann.WithPos a => FromRad Ann.WithPos (Tree a)
instance ToRad   Ann.WithPos a => ToRad   Ann.WithPos (Tree a)

data Leaf a = Leaf
  { value     :: a
  , valueHash :: ByteString
  } deriving (Generic, Show)

instance FromRad Ann.WithPos a => FromRad Ann.WithPos (Leaf a)
instance ToRad   Ann.WithPos a => ToRad   Ann.WithPos (Leaf a)

data Branch a = Branch
  { here  :: ByteString
  , left  :: Tree a
  , right :: Maybe (Tree a)
  } deriving (Generic, Show)

instance FromRad Ann.WithPos a => FromRad Ann.WithPos (Branch a)
instance ToRad   Ann.WithPos a => ToRad   Ann.WithPos (Branch a)

elements :: MerkleTree a -> [a]
elements Empty = []
elements (NonEmpty _ t) = go t
  where
    go  (TBranch b) = go (left b) ++ go' (right b)
    go  (TLeaf l)   = [value l]
    go' (Just t') = go t'
    go' Nothing   = []

instance HasHash (Tree a) where
  hashed (TLeaf l)   = valueHash l
  hashed (TBranch b) = here b

instance HasHash (Maybe (Tree a)) where
  hashed Nothing  = "right-empty"
  hashed (Just t) = hashed t

hashLeaf :: ByteString -> ByteString
hashLeaf x = combi ["leaf", x]

mkLeaf :: HasHash a => a -> Tree a
mkLeaf x = TLeaf Leaf
  { value = x
  , valueHash = hashLeaf (hashed x)
  }

hashBranch :: ByteString -> ByteString -> ByteString
hashBranch x y = combi ["branch", x, y]

mkBranch :: Tree a -> Maybe (Tree a) -> Tree a
mkBranch l r_ = TBranch Branch
  { here = hashBranch (hashed l) (hashed r_)
  , left = l
  , right = r_
  }

mkMerkleTree :: HasHash a => [a] -> MerkleTree a
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

data Claim = At Int Int ByteString

data Side = LeftSide | RightSide deriving (Eq, Show)

newtype ProofElem = ProofElem ByteString deriving (Generic, Show)

instance FromRad Ann.WithPos ProofElem
instance ToRad   Ann.WithPos ProofElem

type Proof = [ProofElem]

proofSize :: Int -> Int
proofSize n = ceiling $ logBase 2 (fromIntegral n :: Double)

-- | Given an (starting at 0) index 'i' of a sequence of length 'n', returns the
-- sequence of sides for the proof, from the root to the leaf.
indexSides :: Int -> Int -> [Side]
indexSides n i = reverse $ take (proofSize n) $ boolToSide <$> bitList i
  where
    bitList x = map (testBit x) [0 .. 8 * sizeOf x - 1]
    boolToSide = \case
      True -> RightSide
      False -> LeftSide

-- | Given an index 'i' and a merkle-tree, returns the proof for the value of the
-- 'i'-th element.
mkProof :: Int -> MerkleTree a -> Maybe (a, Proof)
mkProof _ Empty = Nothing
mkProof i (NonEmpty n _) | i >= n = Nothing
mkProof i (NonEmpty n t) =
  let sides = indexSides n i in
  getProof sides t

getIndex :: (a -> Bool) -> MerkleTree a -> Maybe Int
getIndex p mt = findIndex p (elements mt)

getProof :: [Side] -> Tree a -> Maybe (a, Proof)
getProof (LeftSide:sides) (TBranch b) = do
  (x, rest) <- getProof sides (left b)
  let p = ProofElem $ hashed (right b)
  pure (x, p : rest)
getProof (RightSide:sides) (TBranch b) = do
  r <- right b
  (x, rest) <- getProof sides r
  let p = ProofElem $ hashed (left b)
  pure (x, p : rest)
getProof [] (TLeaf l) = pure (value l, [])
getProof _ _ = Nothing

checkProof
  :: Claim  -- ^ Claim
  -> ByteString     -- ^ Root hash
  -> Proof  -- ^ Proof
  -> Bool
checkProof claim root pf = zipProof claim pf == Just root

zipProof :: Claim -> Proof -> Maybe ByteString
zipProof (At n i el) pf =
    if i < n && length sides == length pf
    then Just . hashMerkleTree n $
           foldr applyPfElem (hashLeaf el) (zip pf sides)
    else Nothing
  where
    sides = indexSides n i
    applyPfElem (ProofElem sib, s) cur = case s of
      LeftSide  -> hashBranch cur sib
      RightSide -> hashBranch sib cur
