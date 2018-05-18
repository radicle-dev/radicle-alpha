{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Radicle.Internal.Chain where

import           Control.Applicative (Alternative)
import           Control.Lens (makeLenses, (&), (.~), (%~), (^.), mapped, (<>~))
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

-- | A helper for running subscribers.
runSubscriber :: Subscriber m a -> Value -> m a
runSubscriber = runReaderT . fromSubscriber

-- | A helper for making subscribers.
makeSubscriber :: (Value -> m a) -> Subscriber m a
makeSubscriber = Subscriber . ReaderT

-- | A blockchain definition.
data Chain m a = Chain
    { _name        :: [Text]
    -- | The fold function.
    , _step        :: Env -> Value -> Either LangError Value
    , _env         :: Env
    -- | How to update the env, given a new value.
    , _updateEnv   :: Value -> Env -> Env
    -- | 'chainSubscribers' is called with each new Env and with evaluated
    -- Value.
    , _subscribers :: [Subscriber m a]
    } deriving (Functor, Generic)

makeLenses ''Chain


instance MFunctor Chain where
    hoist nat chain = chain & subscribers . mapped %~ hoist nat

-- | A simple chain is one where we can ignore all subscribers.
type SimpleChain a = Chain Proxy a

-- | Add a subscriber to a chain. The values are combined with mappend for 'm
-- a'.
addSubscriber :: Chain m a -> Subscriber m a -> Chain m a
addSubscriber chain newCB = chain & subscribers <>~ [newCB]

-- | Removes all subscribers.
removeSubscribers :: Chain m a -> Chain n b
removeSubscribers chain = chain & subscribers .~ mempty

-- | The default genesis chain.
genesisChain :: Chain Identity Value
genesisChain = Chain
    { _name = []
    , _env = mempty
    , _step = \env' val -> runLangM env' (eval val)
    , _updateEnv = upd
    , _subscribers = [Subscriber ask]
    }
  where
    upd :: Value -> Env -> Env
    upd v = case v of
      List [Atom (Ident "set!"), Atom a, val] -> setEnv a val
      _                                       -> id



-- | Fold a chain. All effects are accumulated with a monoid instance.
foldChain :: forall m a t. (Foldable t)
    => Chain m a -> t Value -> Either LangError (Chain m a, [m a])
foldChain chain vals = runWriterT $ foldM runChainStep chain vals
  where
    runChainStep :: Chain m a -> Value -> WriterT [m a] (Either LangError) (Chain m a)
    runChainStep chain' val = do
        let mEvaled = (chain' ^. step) (chain' ^. env) val
        case mEvaled of
            Left err     -> throwError err
            Right evaled ->
                let newChain = chain' & env %~ ((chain' ^. updateEnv) evaled)
                    effs = chain' ^. subscribers
                         & mapped %~ (`runSubscriber` evaled)
                in writer (newChain, effs)


-- | Fold a chain from a source file. The String argument is the name of the
-- source file/block, used for error reporting.
foldChainFromSrc :: Monoid (m a)
    => String -> Text -> Chain m a -> Either LangError (Chain m a, [m a])
foldChainFromSrc sourceName srcCode chain =
    let exprs = first ParseError <$> parseValues sourceName srcCode
    in foldChain chain $ ExceptT exprs
