{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Radicle.Internal.Chain where

import           Control.Applicative (Alternative)
import           Control.Lens (makeLenses, mapped, (%~), (&), (^.))
import           Control.Monad (foldM)
import           Control.Monad.Except (ExceptT(..), throwError)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Identity (Identity(Identity), MonadFix, MonadPlus)
import           Control.Monad.Morph (MFunctor(hoist), MMonad)
import           Control.Monad.Reader (MonadReader, Reader, runReader, reader, ask)
import           Control.Monad.Trans
import           Control.Monad.Writer (WriterT(..), writer)
import           Control.Monad.Trans.Free
import           Data.Bifunctor (first)
import           Data.Proxy (Proxy)
import           Data.Semigroup (Semigroup, (<>))
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Radicle.Internal.Core
import           Radicle.Internal.Parse

-- | Subscribers receive (evaluated) values from a chain, and can perform
-- side-effects.
newtype Subscriber a = Subscriber { fromSubscriber :: Reader Value a }
    deriving ( Generic, Functor, Monad, Applicative, MonadReader Value)

instance Semigroup a => Semigroup (Subscriber a) where
    Subscriber a <> Subscriber b = reader $ \v ->
        runReader a v <> runReader b v

instance (Semigroup a, Monoid a) => Monoid (Subscriber a) where
    mempty = reader $ const mempty
    mappend = (<>)

-- | A helper for running subscribers.
runSubscriber :: Subscriber a -> Value -> a
runSubscriber = runReader . fromSubscriber

-- | A helper for making subscribers.
makeSubscriber :: (Value -> a) -> Subscriber a
makeSubscriber = reader

-- | A blockchain definition.
data Chain a = Chain
    { _name        :: [Text]
    -- | The fold function.
    , _step        :: Env -> Value -> Either LangError Value
    , _env         :: Env
    -- | How to update the env, given a new value.
    , _updateEnv   :: Value -> Env -> Env
    -- | 'chainSubscribers' is called with each new Env and with evaluated
    -- Value.
    , _subscribers :: [Subscriber a]
    } deriving (Functor, Generic)

makeLenses ''Chain

{-instance MFunctor Chain where-}
    {-hoist nat chain = chain & subscribers . mapped %~ hoist nat-}

type ChainF m a = FreeT Chain m a

-- | A simple chain is one where we can ignore all subscribers.
{-type SimpleChain a = Chain Proxy a-}

-- | The default genesis chain.
genesisChain :: Chain Value
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


-- |
foldChainF :: Functor m => ChainF m a -> m ()
foldChainF = foldFreeT _
  {-where-}
    -- Is there no class for this? Seems like MFunctor is almost there...
    {-lame1 :: Subscriber m a -> Subscriber Identity (m a)-}
    {-lame1 f = makeSubscriber $ \v -> Identity $ runSubscriber f v-}

    {-lame2 :: Chain m a -> Chain Identity (m a)-}
    {-lame2 c = c & subscribers . mapped %~ lame1-}

    {-go :: Chain Identity a -> a-}
    {-go c = error "notimpl"-}


{-
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
    -}
