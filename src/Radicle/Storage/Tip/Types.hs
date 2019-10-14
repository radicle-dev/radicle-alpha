{-# LANGUAGE StrictData #-}

module Radicle.Storage.Tip.Types
    ( TipStore (..)
    , hoistTipStore
    , mapTipStoreError

    , TipInfo (..)
    )
where

import           Protolude

import           Data.Aeson (FromJSON, ToJSON)

import           Radicle.Storage.Types (ReaderOrWriter)


-- | The latest known index @tip@ of a 'Radicle.Storage.Machine' idenitified by
-- machine-id @mid@.
data TipInfo mid tip = TipInfo
    { tipMachineId   :: mid
    , tipPointer     :: Maybe tip
    , tipMachineMode :: ReaderOrWriter
    } deriving Generic

instance (ToJSON mid, ToJSON tip) => ToJSON (TipInfo mid tip)
instance (FromJSON mid, FromJSON tip) => FromJSON (TipInfo mid tip)

-- | Storage backend for the current tip of a 'Radicle.Storage.Machine'.
data TipStore e mid tip m = TipStore
    { saveTip  :: mid -> tip -> ReaderOrWriter -> m ()
    -- ^ Store the current @tip@ of the 'Radicle.Storage.Machine' identified by
    -- @mid@ with mode 'ReaderOrWriter'.
    --
    -- This operation MUST be atomic, and guarantee a high degree of durability.
    --
    , readTip  :: mid -> m (Either e (TipInfo mid tip))
    -- ^ Read to 'TipInfo' for 'Radicle.Storage.Machine' @mid@.
    --
    -- It should hold that:
    --
    --  @
    --      saveTip tstore mid tip rw
    --      Right info <- readTip tstore mid
    --      pure $ tipPointer info == Just tip
    --  @
    --
    , listTips :: m [Either e (TipInfo mid tip)]
    -- ^ List all known tips.
    }

hoistTipStore
    :: (forall a. m a -> n a)
    -> TipStore e mid tip m
    -> TipStore e mid tip n
hoistTipStore f t = t
    { saveTip  = \mid tip rw -> f (saveTip t mid tip rw)
    , readTip  = f . readTip t
    , listTips = f (listTips t)
    }

mapTipStoreError
    :: Functor m
    => (e -> e')
    -> TipStore e  mid tip m
    -> TipStore e' mid tip m
mapTipStoreError f t = t
    { readTip  = fmap (first f) . readTip t
    , listTips = map (first f) <$> listTips t
    }
