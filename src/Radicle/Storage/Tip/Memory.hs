module Radicle.Storage.Tip.Memory
    ( ReadError (..)
    , memoryTipStore
    )
where

import           Protolude

import qualified ListT
import qualified StmContainers.Map as STM

import           Radicle.Storage.Tip.Types

{-# ANN module ("HLint: ignore Use newtype instead of data" :: Text) #-}

data ReadError mid
    = NotFound mid

memoryTipStore
    :: (Eq mid, Hashable mid)
    => STM.Map mid (TipInfo mid tip)
    -> TipStore (ReadError mid) mid tip STM
memoryTipStore m = TipStore {..}
  where
    saveTip mid tip rw = STM.insert (TipInfo mid (Just tip) rw) mid m
    readTip mid        = note (NotFound mid) <$> STM.lookup mid m
    listTips           = map (Right . snd) <$> ListT.toList (STM.listT m)
