module Radicle.Internal.Hash where

import           Protolude hiding (hash)

import           Crypto.Hash
import           Data.Copointed (Copointed(..))

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

type BS = ByteString

combi :: [BS] -> BS
combi = foldMap (\x -> "<" <> x <> ">")
  --show . blake2b_256 . mconcat

data MerkleTree
  = Empty
  | NonEmpty Tree
  deriving (Show)

data Tree
  = TBranch Branch
  | TLeaf Leaf
  deriving (Show)

data Leaf = Leaf BS
  deriving (Show)

data Branch = Branch
  { here  :: BS
  , size  :: Word32
  , left  :: Tree
  , right :: Tree
  }
  deriving (Show)

treeHash :: Tree -> BS
treeHash (TLeaf (Leaf h)) = h
treeHash (TBranch b)      = here b

treeSize :: Tree -> Word32
treeSize (TBranch b) = size b
treeSize (TLeaf _)   = 1

mkLeaf :: BS -> Tree
mkLeaf x = TLeaf . Leaf . combi $ ["leaf", x]

mkBranch :: Tree -> Tree -> Tree
mkBranch l r = TBranch Branch
  { here = combi ["branch", treeHash l, treeHash r]
  , size = treeSize l + treeSize r
  , left = l
  , right = r
  }

-- | Return the largest power of two such that it's smaller than n.
powerOfTwo :: (Bits a, Num a) => a -> a
powerOfTwo n
   | n .&. (n - 1) == 0 = n `shiftR` 1
   | otherwise = go n
 where
   go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))

mkMerkleTree :: [BS] -> MerkleTree
mkMerkleTree [] = Empty
mkMerkleTree xs' = NonEmpty $ go (length xs') xs'
  where
    go _ [x] = mkLeaf x
    go n xs = mkBranch (go a ys) (go b zs)
      where
        a = powerOfTwo n
        b = n - a
        (ys, zs) = splitAt a xs

data Side = LeftSide | RightSide

data ProofElem = ProofElem
  {
    -- me      :: BS
    sibling :: BS
  , side    :: Side
  }

type Proof = [ProofElem]

mkProof :: Word32 -> MerkleTree -> BS -> Maybe Proof
mkProof i t e = case t of
  Empty -> Nothing
  NonEmpty (TLeaf (Leaf h)) ->
    if h == e then Just [] else Nothing
  NonEmpty (TBranch b) ->
    let n = size b in
    if n < i
    then Nothing
    else
      let a = powerOfTwo n in
      if i <= a
        then do
          rest <- mkProof i (NonEmpty (left b)) e
          let p = ProofElem
                    {
                      -- me = here b
                      sibling = treeHash (right b)
                    , side = LeftSide
                    }
          pure $ p : rest
        else do
          rest <- mkProof (i - a) (NonEmpty (right b)) e
          let p = ProofElem
                    {
                      -- me = here b
                      sibling = treeHash (left b)
                    , side = RightSide
                    }
          pure $ p : rest

-- checkProof :: Proof -> BS -> BS -> Bool
-- checkProof (p:ps) root elem = case side p of
--   LeftSide -> _
--   RightSide -> combi ["branch", sibling p, ]

rootFromProof :: Proof -> BS -> BS
rootFromProof [] h = combi ["leaf", h]
rootFromProof (p:ps) s = case side p of
  LeftSide ->
    combi ["branch", rootFromProof ps s, sibling p]
  RightSide ->
    combi ["branch", sibling p, rootFromProof ps s]

-- data Merkle = Merkle

-- mkMerkle :: Value -> Merkle
-- mkMerkle = _

-- data Loc
--   = NthOfList Word32
--   | NthOfVec Word32
--   | KeyOfDict Value

-- type Path = [Loc]

-- data Proof = Proof

-- mkProof :: Path -> Value -> Value -> Maybe Proof
-- mkProof = notImplemented

-- checkProof :: Path -> BS -> BS -> Bool
-- checkProof = notImplemented
