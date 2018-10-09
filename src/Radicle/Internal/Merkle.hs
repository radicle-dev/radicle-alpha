module Radicle.Internal.Merkle
  ( HasHash(..)
  , mkMerkleTree
  , mkProof
  , checkProof
  , Claim(..)
  , Proof
  , MerkleTree
  , combi
  , getIndex
  , hashMerkleTree
  , zipProof
  ) where

import           Protolude hiding (hash)

import           Data.List (findIndex)
import           Foreign.Storable

import           Radicle.Internal.Hash

type BS = ByteString

combi :: [BS] -> BS
combi =
  hash . mconcat
  --foldMap (\x -> "(" <> x <> ")")

class HasHash a where
  hashed :: a -> BS

-- | A Merkle hash tree containing values 'a' at the leaves.
data MerkleTree a
  = Empty
  | NonEmpty Int (Tree a)
  deriving (Show)

-- merkleTreeHash :: MerkleTree a -> Maybe BS
-- merkleTreeHash = \case
--   Empty -> Nothing
--   NonEmpty _ t -> pure $ hashed t

hashMerkleTree :: Int -> BS -> BS
hashMerkleTree n x = combi ["non-empty-tree", show n, x]

instance HasHash (MerkleTree a) where
  hashed Empty          = "empty-tree"
  hashed (NonEmpty n t) = hashMerkleTree n (hashed t)

data Tree a
  = TBranch (Branch a)
  | TLeaf (Leaf a)
  deriving (Show)

data Leaf a = Leaf
  { value     :: a
  , valueHash :: BS
  } deriving (Show)

data Branch a = Branch
  { here  :: BS
  , left  :: Tree a
  , right :: Maybe (Tree a)
  } deriving (Show)

elements :: MerkleTree a -> [a]
elements Empty = []
elements (NonEmpty _ t) = go t
  where
    go (TBranch b) = go (left b) ++ go' (right b)
    go (TLeaf l)   = [value l]
    go' (Just t') = go t'
    go' Nothing   = []

instance HasHash (Tree a) where
  hashed (TLeaf l)   = valueHash l
  hashed (TBranch b) = here b

instance HasHash (Maybe (Tree a)) where
  hashed Nothing  = "right-empty"
  hashed (Just t) = hashed t

hashLeaf :: BS -> BS
hashLeaf x = combi ["leaf", x]

mkLeaf :: HasHash a => a -> Tree a
mkLeaf x = TLeaf Leaf
  { value = x
  , valueHash = hashLeaf (hashed x)
  }

hashBranch :: BS -> BS -> BS
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

data Claim = At Int Int BS

data Side = LeftSide | RightSide deriving (Eq, Show)

newtype ProofElem = ProofElem BS deriving (Show)

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
  -> BS     -- ^ Root hash
  -> Proof  -- ^ Proof
  -> Bool
checkProof claim root pf = zipProof claim pf == Just root

zipProof :: Claim -> Proof -> Maybe BS
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

-- Testing

-- newtype By = By ByteString
--   deriving (Show)

-- instance HasHash By where
--   hashed (By x) = x

--foo :: [By] -> Bool
-- foo xs =
--   let mtree = mkMerkleTree xs
--       rootHash = hashed mtree
--       n = length xs
--       is = [0..(n-1)]
--       xsi = zip xs is
--       claims = [ At n i' (hashed x) | (x, i') <- xsi ]
--       proofs = sequence [ mkProof i' mtree | i' <- is ]
--   in case proofs of
--     Nothing -> panic "bad"
--     Just ps' -> (rootHash, [ zipProof c p | (c, p) <- zip claims (snd <$> ps') ])
--       -- let ps = snd <$> ps'
--       --     result = and [ checkProof c rootHash p | (c, p) <- zip claims ps ]
--       -- in result == True
