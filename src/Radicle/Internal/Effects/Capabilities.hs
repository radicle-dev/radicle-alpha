-- | This module defines fine-grained monad classes for effects.
--
-- The intent is that any set of primops may wear on their sleaves (i.e.
-- constraints) what effects they do.
{-# LANGUAGE CPP #-}
module Radicle.Internal.Effects.Capabilities where

import           Protolude

import           Data.Text.Prettyprint.Doc (PageWidth)
import           System.Console.Haskeline
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.XMLHttpRequest
                 (getResponseText, newXMLHttpRequest, openSimple, send)
#endif

import           Radicle.Internal.Core

class (Monad m) => Stdin m where
    getLineS :: m (Maybe Text)  -- gives Nothing on EOF
instance {-# OVERLAPPABLE #-} Stdin m => Stdin (Lang m) where
    getLineS = lift getLineS
instance (MonadException m) => Stdin (InputT m) where
    getLineS = (fmap.fmap) toS (getInputLine "rad> ")

class (Monad m) => Stdout m where
    putStrS :: Text -> m ()
instance {-# OVERLAPPABLE #-} Stdout m => Stdout (Lang m) where
    putStrS = lift . putStrS
instance (MonadException m, Monad m) => Stdout (InputT m) where
    putStrS = outputStrLn . toS

class (Monad m) => Exit m where
    exitS :: m ()
instance Exit m => Exit (Lang m) where
    exitS = lift exitS

class (Monad m) => GetEnv m r | m -> r where
    getEnvS :: m (Env r)
instance Monad m => GetEnv (Lang m) Value where
    getEnvS = gets bindingsEnv

class (Monad m) => SetEnv m r | m -> r where
    setEnvS :: Env r -> m ()
instance Monad m => SetEnv (Lang m) Value where
    setEnvS e = modify (\bnds -> bnds {bindingsEnv = e})

class (Monad m) => GetSourceName m where
    getSourceNameS :: m Text
class (Monad m) => HasPageWidth m where
    getPageWidthS :: m PageWidth
class (Monad m) => GetSubs m where
    getSubsS :: m [(Text, Value -> m ())]
class (Monad m) => SetSubs m where
    setSubS :: Text -> (Value -> m ()) -> m ()

class (Monad m) => ReadFile m where
    readFileS :: Text -> m Text
#ifdef ghcjs_HOST_OS
instance ReadFile (InputT IO) where
    readFileS = lift . requestFile
      where
        requestFile :: Text -> IO Text
        requestFile filename = do
            req <- newXMLHttpRequest
            openSimple req ("GET" :: Text) filename
            send req
            resp <- getResponseText req
            pure $ case resp of
                Nothing -> ""
                Just v  -> v
#else
instance ReadFile (InputT IO) where
    readFileS = lift . readFile . toS
#endif
instance {-# OVERLAPPABLE #-} ReadFile m => ReadFile (Lang m) where
    readFileS = lift . readFileS


putStrLnS :: (Stdout m) => Text -> m ()
putStrLnS t = putStrS t >> putStrS "\n"

modifyEnvS :: (GetEnv m r, SetEnv m r) => (Env r -> Env r) -> m ()
modifyEnvS f = getEnvS >>= setEnvS . f
