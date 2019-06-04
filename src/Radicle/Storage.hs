module Radicle.Storage
    ( Machines
    , withMachines

    , lookupMachine
    , addMachine
    , removeMachine
    , updateMachine

    -- * Re-exports
    , Machine
    , machineId
    , machineLastIndex
    , machineState
    , machineMode

    , Backend (..)
    , Nonce
    , Inputs
    , Outputs
    , Expressions (..)
    , MachineId
    , MachineIndex
    , ReaderOrWriter (..)
    )
where

import           Protolude hiding
                 (MVar, atomically, bracket, newEmptyMVar, withMVar)

import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Trans.Maybe
import qualified ListT
import qualified StmContainers.Map as STM
import           UnliftIO (MonadUnliftIO, atomically, bracket)

import           Radicle (Value)
import           Radicle.Storage.Machine hiding (updateMachine)
import qualified Radicle.Storage.Machine as Storage
import           Radicle.Storage.PubSub
import           Radicle.Storage.Tip
import           Radicle.Storage.Types


data Machines e mid idx m = Machines
    { machines :: STM.Map mid (MEnv e mid idx)
    , settings :: Settings
    , backend  :: Backend  e mid idx m
    , tips     :: TipStore e mid idx m
    , pubsub   :: PubSub   e mid     m
    }

-- | Initialise and run the stateful machine cache and storage.
--
-- Known machines are loaded from the 'TipStore'.
--
withMachines
    :: ( MachineId     mid
       , MonadUnliftIO m
       )
    => Settings
    -> Backend  e mid idx m
    -> TipStore e mid idx m
    -> PubSub   e mid     m
    -> (Machines e mid idx m -> m a)
    -> m a
withMachines settings backend tips pubsub k =
    bracket aquire release $ \mmap -> k Machines { machines = mmap, ..}
  where
    aquire = do
        -- TODO(kim): what to do with machines which failed to initialise?
        infos <- listTipsIgnore tips
        ms    <-
            -- TODO(kim): do this concurrently
            for infos $ \TipInfo {..} -> do
                m <- runMachine settings
                                backend
                                tips
                                tipMachineId
                                tipPointer
                                tipMachineMode
                m <$ for_ m (subscribe pubsub tipMachineId . Storage.updateMachine)
        atomically $ do
            mmap <- STM.new
            for_ (rights ms) $ \machine -> do
                mid <- machineId <$> getMachine machine
                STM.insert machine mid mmap
            pure mmap

    release mmap = do
        ms <- atomically $ ListT.toList (STM.listT mmap)
        for ms $ \(mid, machine) ->
            shutdownMachine machine *> unsubscribe pubsub mid

lookupMachine
    :: (Eq mid, Hashable mid, MonadIO m)
    => Machines e mid idx m
    -> mid
    -> m (Maybe (Machine mid idx))
lookupMachine Machines { machines } mid =
    atomically $ STM.lookup mid machines >>= traverse getMachine

-- modifyMachine
-- traverse

addMachine
    :: ( MachineId     mid
       , MonadUnliftIO m
       , MonadThrow    m
       )
    => Machines e mid idx m
    -> mid
    -> Maybe idx
    -> ReaderOrWriter
    -> m ()
addMachine
    Machines { machines, settings, backend, tips, pubsub }
    mid idx mode
    = do
    machine <-
        either throw pure
            =<< runMachine settings backend tips mid idx mode
    have    <-
        atomically $ do
            have <- isJust <$> STM.lookup mid machines
            have <$ unless have (STM.insert machine mid machines)
    if have then
        shutdownMachine machine
    else
        subscribe pubsub mid $ Storage.updateMachine machine

removeMachine
    :: (Eq mid, Hashable mid, MonadUnliftIO m)
    => Machines e mid idx m
    -> mid
    -> m ()
removeMachine Machines { machines, pubsub } mid = do
    machine <-
        atomically $ do
            machine <- STM.lookup mid machines
            machine <$ for_ machine (const $ STM.delete mid machines)
    for_ machine $ \m ->
        shutdownMachine m *> unsubscribe pubsub mid

-- | Update machine @mid@ with new inputs.
--
-- Returns 'Nothing' if:
--   * the machine was added as a 'Reader' (update is a no-op)
--   * the machine is not known
--
-- Otherwise, 'Just' a 'TaskResult' is returned, which can be used to block on
-- the results.
--
updateMachine
    :: (Eq mid, Hashable mid, MonadUnliftIO m)
    => Machines e mid idx m
    -> mid
    -> [Value]
    -> Maybe Nonce
    -> m (Maybe (TaskResult e m))
updateMachine Machines { machines } mid inputs nonce = runMaybeT $ do
    menv <- MaybeT $ atomically $ STM.lookup mid machines
    MaybeT $ do
        machine <- atomically $ getMachine menv
        case machineMode machine of
            Reader -> pure Nothing
            Writer -> Just <$> Storage.updateMachine menv (Expressions inputs nonce)
