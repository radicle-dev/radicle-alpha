-- | This module defines fine-grained monad classes for effects.
--
-- The intent is that any set of primops may wear on their sleaves (i.e.
-- constraints) what effects they do.
{-# LANGUAGE CPP #-}
module Radicle.Internal.Effects.Capabilities where

import           Protolude

import qualified Data.Map as Map
import           Data.Text.Prettyprint.Doc (PageWidth)
import           System.IO (isEOF)
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.XMLHttpRequest
                 (getResponseText, newXMLHttpRequest, openSimple, send)
#endif

import           Radicle.Internal.Core
import qualified Radicle.Internal.Input as Input

class (Monad m) => Stdin m where
    getLineS :: m (Maybe Text)  -- gives Nothing on EOF
    getLineCompletionS :: [Text] -> m (Maybe Text)
instance {-# OVERLAPPABLE #-} Stdin m => Stdin (Lang m) where
    getLineS = lift getLineS
    getLineCompletionS = lift . getLineCompletionS
instance Stdin IO where
    getLineS = do
        done <- isEOF
        if done
        then pure Nothing
        else Just <$> getLine
    getLineCompletionS _ = getLineS
instance (Input.MonadException m) => Stdin (Input.InputT m) where
    getLineS = Input.getInputLine Input.noCompletions "rad> "
    getLineCompletionS compl = Input.getInputLine (radicleCompletion compl) "rad> "


radicleCompletion :: (Monad m) => [Text] -> Input.Completions m
radicleCompletion = Input.wordCompletions ['(', ')', ' ', '\n'] . (specials <>)
    where
    specials = fromIdent <$> Map.keys (specialForms @Identity)

class (Monad m) => Stdout m where
    putStrS :: Text -> m ()
instance {-# OVERLAPPABLE #-} Stdout m => Stdout (Lang m) where
    putStrS = lift . putStrS
instance Stdout IO where
    putStrS = putStrLn
instance (Input.MonadException m, Monad m) => Stdout (Input.InputT m) where
    putStrS = Input.outputStrLn . toS

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
    readFileS :: Text -> m (Either Text Text)  -- ^ Left error or Right contents
#ifdef ghcjs_HOST_OS
instance ReadFile (Input.InputT IO) where
    readFileS = lift . requestFile
      where
        requestFile :: Text -> IO (Either Text Text)
        requestFile filename = do
            req <- newXMLHttpRequest
            openSimple req ("GET" :: Text) filename
            send req
            resp <- getResponseText req
            pure $ case resp of
                Nothing -> Left "no response from server"
                Just v  -> Right v
#else
instance ReadFile IO where
    readFileS fname = (Right <$> readFile (toS fname))
                        `catch` (\(e :: IOException) -> pure (Left (show e)))
instance ReadFile (Input.InputT IO) where
    readFileS fname = lift $ (Right <$> readFile (toS fname))
                               `catch` (\(e :: IOException) -> pure (Left (show e)))
#endif
instance {-# OVERLAPPABLE #-} ReadFile m => ReadFile (Lang m) where
    readFileS = lift . readFileS

putStrLnS :: (Stdout m) => Text -> m ()
putStrLnS t = putStrS t >> putStrS "\n"

modifyEnvS :: (GetEnv m r, SetEnv m r) => (Env r -> Env r) -> m ()
modifyEnvS f = getEnvS >>= setEnvS . f
