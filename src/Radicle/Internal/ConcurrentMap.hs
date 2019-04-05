module Radicle.Internal.ConcurrentMap
  ( empty
  , lookup
  , nonAtomicRead
  , insert
  , modifyExistingValue
  , modifyValue
  , CMap
  ) where

import           Protolude hiding (empty)

import           Control.Monad.IO.Unlift
import qualified Data.Map.Strict as Map

-- | A mutable Map that serializes operations on the same key.
--
-- Calls to the 'lookup', 'insert', 'modifyValue', and
-- 'modifyExistingValue' with the same key are serialized. Calls to
-- these functions with different keys can run concurrently.
newtype CMap k v = CMap (MVar (Map k (MVar (Maybe v))))

-- | Create a new empty 'CMap'.
empty :: IO (CMap k v)
empty = CMap <$> newMVar Map.empty

-- | Atomically lookup a value.
lookup :: Ord k => k -> CMap k v -> IO (Maybe v)
lookup k (CMap m_) = withMVar m_ $ \m -> do
   case Map.lookup k m of
     Nothing -> pure Nothing
     Just v_ -> readMVar v_

-- | Non-atomically read the contents of a 'CMap'. Provides a
-- consistent shapshot of which keys were present at some
-- time. However, the values might be snapshotted at different times.
nonAtomicRead :: CMap k v -> IO (Map k v)
nonAtomicRead (CMap m_) = do
  m <- readMVar m_
  Map.mapMaybe identity <$> traverse readMVar m

-- | Atomically insert a key-value pair into a 'CMap'.
insert :: Ord k => k -> v -> CMap k v -> IO ()
insert k v (CMap m_) = modifyMVar m_ $ \m -> do
  v_ <- newMVar (Just v)
  let m' = Map.insert k v_ m
  pure (m', ())

-- | Atomically modifies a value associated to a key but only if it
-- exists. Blocks all operations on @k@ while @f@ is running.
modifyExistingValue :: (MonadUnliftIO m, Ord k) => k -> CMap k v -> (v -> m (v, a)) -> m (Maybe a)
modifyExistingValue k c f =
    modifyValue k c $ \case
        Nothing -> pure (Nothing, Nothing)
        Just v -> do
            (v', a) <- f v
            pure (Just v', Just a)

-- | Atomically modifies a value associated to a key. Blocks all
-- operations on @k@ while @f@ is running.
modifyValue :: forall k v a m. (MonadUnliftIO m, Ord k) => k -> CMap k v -> (Maybe v -> m (Maybe v, a)) -> m a
modifyValue k (CMap m_) f = withRunInIO $ \runInIO -> do
    valueVar <- modifyMVar m_ $ \m ->
        case Map.lookup k m of
            Nothing -> do
                valueVar <- newMVar Nothing
                pure (Map.insert k valueVar m, valueVar)
            Just valueVar -> do
                pure (m, valueVar)
    -- To prevent space leaks we probably should remove the key from
    -- the map if @maybeValue'@ is 'Nothing'. We need to be very
    -- careful if we implement this: If we remove an MVar from the map
    -- another `modifyValue` thread might still have a reference to it
    -- and run `modifyMVar`.
    modifyMVar valueVar (runInIO . f)
