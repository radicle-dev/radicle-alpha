{-# LANGUAGE StrictData #-}

module Radicle.Storage.Tip
    ( TipInfo (..)
    , TipStore
    , saveTip
    , readTip
    , listTips
    , listTipsThrow
    , listTipsIgnore

    , fsTipStore
    )
where

import           Protolude

import           Control.Exception.Safe (MonadThrow, throw)

import           Radicle.Storage.Tip.File
import           Radicle.Storage.Tip.Types


listTipsThrow
    :: (Exception e, MonadThrow m)
    => TipStore e mid tip m
    -> m [TipInfo mid tip]
listTipsThrow TipStore { listTips } = do
    tips <- listTips
    case partitionEithers tips of
        (e:_, _ ) -> throw e
        ([] , xs) -> pure xs

listTipsIgnore :: Functor m => TipStore e mid tip m -> m [TipInfo mid tip]
listTipsIgnore TipStore { listTips } = rights <$> listTips
