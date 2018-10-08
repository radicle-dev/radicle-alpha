module Radicle.Internal.Hash
  ( hashRad
  , merkleTreeHash
  , mkMerkleTree
  , mkMerkleProof
  , checkMerkleProof
  , mkProof
  ) where

import           Protolude hiding (hash)

import           Crypto.Hash
import           Data.Copointed (Copointed(..))
import qualified Data.Map.Strict as Map
import           Foreign.Storable

import           Radicle.Internal.Annotation
import           Radicle.Internal.Core
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty

-- = Hashing of radicle values

blake2b_256 :: ByteString -> Digest Blake2b_256
blake2b_256 = hash

hashText :: Text -> Text
hashText = show . blake2b_256 . toS

hashRad :: (Copointed t, Annotation t) => Annotated t ValueF -> Text
hashRad = hashText . renderCompactPretty

-- = Merkle trees

class HasHash a where
  hashed :: a -> BS

type BS = ByteString

combi :: [BS] -> BS
combi =
  --show . blake2b_256 . mconcat
  foldMap (\x -> "(" <> x <> ")")

data MerkleTree a
  = Empty
  | NonEmpty Int (Tree a)
  deriving (Show)

merkleTreeHash :: MerkleTree a -> Maybe BS
merkleTreeHash = \case
  Empty -> Nothing
  NonEmpty _ t -> pure $ hashed t

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

data Side = LeftSide | RightSide deriving (Eq, Show)

data MerkleProofElem = ProofElem
  { sibling :: BS
  , side    :: Side
  } deriving (Show)

type MerkleProof = [MerkleProofElem]

-- | Given an (starting at 0) index 'i' of a sequence of length 'n', returns the
-- sequence of sides for the proof, from the leaf to the root.
indexSides :: Int -> Int -> [Side]
indexSides n i = take b $ boolToSide <$> bitList i
  where
    b = ceiling $ logBase 2 (fromIntegral n :: Double)
    bitList x = map (testBit x) [0..8*(sizeOf x)-1]
    boolToSide = \case
      True -> RightSide
      False -> LeftSide

-- | Given an index 'i' and a merkle-tree, returns the proof for the value of the
-- 'i'-th element.
mkMerkleProof :: Int -> MerkleTree a -> Maybe (a, MerkleProof)
mkMerkleProof _ Empty = Nothing
mkMerkleProof i (NonEmpty n _) | i >= n = Nothing
mkMerkleProof i (NonEmpty n t) =
  let sides = reverse (indexSides n i) in
  getProof sides t

mkMerkleInclusionProof :: forall a. (a -> Bool) -> MerkleTree a -> Maybe (a, MerkleProof)
mkMerkleInclusionProof _ Empty = Nothing
mkMerkleInclusionProof p (NonEmpty _ t) = do
    ss <- findElem t
    getProof ss t
  where
    findElem = \case
      TLeaf l | p (value l) -> pure []
      TBranch b -> do
        case findElem (left b) of
          Just sides -> pure $ LeftSide : sides
          Nothing -> do
            r <- right b
            sides <- findElem r
            pure (RightSide : sides)
      _ -> Nothing

getProof :: [Side] -> Tree a -> Maybe (a, MerkleProof)
getProof (LeftSide:sides) (TBranch b) = do
  (x, rest) <- getProof sides (left b)
  let p = ProofElem {sibling = hashed (right b), side = LeftSide }
  pure (x, p : rest)
getProof (RightSide:sides) (TBranch b) = do
  r <- right b
  (x, rest) <- getProof sides r
  let p = ProofElem {sibling = hashed (left b), side = RightSide}
  pure (x, p : rest)
getProof [] (TLeaf l) = pure (value l, [])
getProof _ _ = Nothing

-- | Checks a proof 'pf' that element at index 'i' in sequence of total length
-- 'n' is indeed 'el'.
checkMerkleProof
  :: Int -- ^ Sequence size
  -> Int -- ^ Index
  -> BS     -- ^ Root hash
  -> BS     -- ^ Element
  -> MerkleProof  -- ^ Proof
  -> Bool
checkMerkleProof n i root el pf =
     i < n
  && reverse (indexSides n i) == (side <$> pf)
  && zipMerkleProof el pf == root

zipMerkleProof :: BS -> MerkleProof -> BS
zipMerkleProof el pf = foldr applyPfElem (hashLeaf el) pf
  where
    applyPfElem p x = case side p of
      LeftSide  -> hashBranch x (sibling p)
      RightSide -> hashBranch (sibling p) x

-- = Authenticated radicle

data Loc = NthOfVec Int Int | HasKeyVal Int BS deriving (Show)

data Claim = Claim [Loc] BS deriving (Show)

data RadProofElem
  = PfNthOfVec MerkleProof
  | PfHasKey MerkleProof
  deriving (Show)

type RadProof = [RadProofElem]

data HashData
  =
  -- | A simple value
    HashValue BS
  -- | A vector of nested values
  | HashVec (MerkleTree HashData)
  -- | A dict of nested values, represented as key-value pairs, with hashed key.
  | HashDict (MerkleTree (BS, HashData))
  deriving (Show)

instance HasHash HashData where
  hashed = \case
    HashValue h -> hashElem h
    HashVec t -> hashVec (hashed t)
    HashDict t -> hashDict (hashed t)

hashElem, hashVec, hashDict :: BS -> BS
hashElem x = combi ["elem", x]
hashVec x = combi ["vector", x]
hashDict x = combi ["dict", x]

hashKeyValue :: BS -> BS -> BS
hashKeyValue k v = combi ["key-value", k, v]

instance HasHash (BS, HashData) where
  hashed (k, v) = hashKeyValue k (hashed v)

mkProof :: HashData -> Claim -> Maybe RadProof
mkProof (HashValue h) (Claim [] h') =
  if h == h' then Just [] else Nothing
mkProof (HashVec t) (Claim (NthOfVec _ i : locs) h') = do
  (x, pf) <- mkMerkleProof i t
  rest <- mkProof x (Claim locs h')
  pure (PfNthOfVec pf : rest)
mkProof (HashDict t) (Claim (HasKeyVal n k : locs) h) = do
    ((_, x), pf) <- mkMerkleInclusionProof isKey t
    rest <- mkProof x (Claim locs h)
    pure (PfHasKey pf : rest)
  where
    isKey (k', _) = k == k'
mkProof _ _ = Nothing

zipProof :: Claim -> RadProof -> BS -> Maybe BS
zipProof (Claim [] h) [] el
  | hashElem h == hashElem el = pure $ hashElem el
  | otherwise = Nothing
zipProof (Claim (loc:locs) h) (x:pf) el = case (loc, x) of
  (NthOfVec n i, PfNthOfVec p) -> do
    h <- zipProof (Claim locs h) pf el
    pure . hashVec . hashMerkleTree n $ zipMerkleProof h p
  (HasKeyVal n k, PfHasKey p) -> do
    h <- zipProof (Claim locs h) pf el
    pure . hashDict . hashMerkleTree n $ zipMerkleProof (hashKeyValue k h) p
  _ -> Nothing

checkProof :: Claim -> RadProof -> BS -> BS -> Bool
checkProof claim pf el rootHash =
  zipProof claim pf el == Just rootHash

toHashData :: Value -> Maybe HashData
toHashData = \case
    Vec xs -> do
      ys <- traverse toHashData (toList xs)
      let t = mkMerkleTree ys
      pure $ HashVec t
    Dict m -> do
      let kvs = Map.toList m
      ys <- traverse toHashData (snd <$> kvs)
      let ks = hashValue . fst <$> kvs
      let t = mkMerkleTree (zip ks ys)
      pure $ HashDict t
    v -> Just . HashValue $ hashValue v

hashValue :: Value -> BS
hashValue = --show . blake2b_256 . toS
  toS . renderCompactPretty

-- Testy stuff

--foo :: Maybe Value
foo = do
    xs <- val "[:a 1 {:foo 42 :bar 2}]"
    a <- val ":a"
    kFoo <- val ":foo"
    fourtyTwo <- val "42"
    hd <- toHashData xs
    let rootHash = hashed hd
    let claim = Claim [NthOfVec 3 2, HasKeyVal 2 (hashValue kFoo)] (hashValue fourtyTwo)
    pf <- mkProof hd claim
    z <- zipProof claim pf (hashValue fourtyTwo)
    --pure (z, rootHash)
    --pure (z == rootHash)
    pure (checkProof claim pf (hashValue fourtyTwo) rootHash)
  where
    val :: Text -> Maybe Value
    val t = either (const Nothing) Just (parse "" t)
