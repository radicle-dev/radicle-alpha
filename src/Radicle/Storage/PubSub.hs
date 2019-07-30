module Radicle.Storage.PubSub
    ( PubSub (..)
    , hoistPubSub
    , mapPubSubError
    )
where

import           Protolude

import           Radicle.Storage.Machine
                 (TaskResult, hoistTaskResult, mapTaskResultError)
import           Radicle.Storage.Types (Inputs)


-- | A 'PubSub' allows to apply 'Inputs' to a 'Radicle.Storage.Machine' from
-- remote instances.
data PubSub e mid m = PubSub
    { subscribe   :: mid -> (Inputs -> m (TaskResult e m)) -> m ()
    , unsubscribe :: mid -> m ()
    }

hoistPubSub
    :: Functor n
    => (forall a. m a -> n a)
    -> (forall a. n a -> m a) -- maybe not
    -> PubSub e mid m
    -> PubSub e mid n
hoistPubSub f g p = p
    { subscribe   = \mid act -> f $ subscribe p mid (hoistAction g act)
    , unsubscribe = f . unsubscribe p
    }

hoistAction
    :: Functor m
    => (forall a. m a -> n a)
    -> (Inputs -> m (TaskResult e m))
    -> (Inputs -> n (TaskResult e n))
hoistAction f g = f . fmap (hoistTaskResult f) . g

mapPubSubError
    :: Functor m
    => (e' -> e) -- lol no. need a better way to unify the error types
    -> PubSub e  mid m
    -> PubSub e' mid m
mapPubSubError f p = p
    { subscribe = \mid act -> subscribe p mid (mapActionError f act)
    }

mapActionError
    :: Functor m
    => (e -> e')
    -> (Inputs -> m (TaskResult e  m))
    -> (Inputs -> m (TaskResult e' m))
mapActionError f g = fmap (mapTaskResultError f) . g
