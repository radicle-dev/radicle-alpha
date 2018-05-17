{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Internal.Chain where

import           Control.Applicative (Alternative)
import           Control.Monad (foldM)
import           Control.Monad.Except (ExceptT(..), throwError)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Identity (Identity, MonadFix, MonadPlus)
import           Control.Monad.Morph (MFunctor(hoist), MMonad)
import           Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import           Control.Monad.Trans
import           Control.Monad.Writer (WriterT(..), writer)
import           Data.Bifunctor (first)
import           Data.Proxy (Proxy)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Radicle.Internal.Core
import           Radicle.Internal.Parse

-- | Subscribers receive (evaluated) values from a chain, and can perform
-- side-effects.
newtype Subscriber m a = Subscriber { fromSubscriber :: ReaderT Value m a }
    deriving ( Generic, Functor, Monad, Applicative, MonadIO, MonadReader Value
             , Alternative, MonadPlus, MonadFix, MonadTrans, MonadFail, MFunctor
             , MMonad)

instance Semigroup (m a) => Semigroup (Subscriber m a) where
    Subscriber a <> Subscriber b = Subscriber . ReaderT $ \v ->
        runReaderT a v <> runReaderT b v

instance (Semigroup (m a), Monoid (m a)) => Monoid (Subscriber m a) where
    mempty = Subscriber . ReaderT $ const mempty
    mappend = (<>)

runSubscriber :: Subscriber m a -> Value -> m a
runSubscriber = runReaderT . fromSubscriber

makeSubscriber :: (Value -> m a) -> Subscriber m a
makeSubscriber = Subscriber . ReaderT

-- | A blockchain definition.
data Chain m a = Chain
    { chainName       :: [Text]
    -- | The fold function.
    , chainStep       :: Env -> Value -> Either LangError Value
    , chainEnv        :: Env
    -- | How to update the env, given a new value.
    , chainUpdateEnv  :: Value -> Env -> Env
    -- | 'chainSubscriber' is called with each new Env and with evaluated
    -- Value.
    , chainSubscriber :: Subscriber m a
    } deriving (Functor, Generic)


instance MFunctor Chain where
    hoist nat chain =
        chain { chainSubscriber = hoist nat $ chainSubscriber chain }

-- | A simple chain is one where we can ignore all subscribers.
type SimpleChain a = Chain Proxy a

-- | Add a subscriber to a chain. The values are combined with mappend for 'm
-- a'.
addSubscriber :: Semigroup (m a) => Chain m a -> Subscriber m a -> Chain m a
addSubscriber chain newCB
    = chain { chainSubscriber = chainSubscriber chain <> newCB }

-- | Removes all subscribers.
removeSubscribers :: (Semigroup (n b), Monoid (n b)) => Chain m a -> Chain n b
removeSubscribers chain = chain { chainSubscriber = mempty }

-- | The default genesis chain.
genesisChain :: Chain Identity Value
genesisChain = Chain
    { chainName = []
    , chainEnv = mempty
    , chainStep = \env val -> runLangM env (eval val)
    , chainUpdateEnv = updateEnv
    , chainSubscriber = Subscriber ask
    }

updateEnv :: Value -> Env -> Env
updateEnv v = case v of
  List [Atom "set!", Atom a, val] -> setEnv a val
  _                               -> id


-- | Fold a chain. All effects are accumulated with a monoid instance.
foldChain :: forall m a t. (Monoid (m a), Foldable t)
    => Chain m a -> t Value -> Either LangError (Chain m a, m a)
foldChain chain vals = runWriterT $ foldM runChainStep chain vals
  where
    runChainStep :: Chain m a -> Value -> WriterT (m a) (Either LangError) (Chain m a)
    runChainStep chain'@Chain{..} val = do
        let mEvaled = chainStep chainEnv val
        case mEvaled of
            Left err     -> throwError err
            Right evaled ->
                let newChain = chain' { chainEnv = chainUpdateEnv evaled chainEnv }
                in writer (newChain, runSubscriber chainSubscriber evaled)


-- | Fold a chain from a source file. The String argument is the name of the
-- source file/block, used for error reporting.
foldChainFromSrc :: Monoid (m a)
    => String -> Text -> Chain m a -> Either LangError (Chain m a, m a)
foldChainFromSrc sourceName srcCode chain =
    let exprs = first ParseError <$> parseValues sourceName srcCode
    in foldChain chain $ ExceptT exprs
