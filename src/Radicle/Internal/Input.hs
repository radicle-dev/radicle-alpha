-- | Haskeline-style wrapper, since it doesn't *quite* provide the right API
-- (completion functions cannot be dynamically modified).  It will also enable
-- a nice abstraction barrier for REPL UI features.

module Radicle.Internal.Input
    ( InputT
    , Completions
    , noCompletions
    , wordCompletions
    , getInputLine
    , runInputT
    , outputStrLn

    , MonadException
    )
where

import           Protolude

import           Control.Monad.Trans (MonadTrans(..))
import qualified Data.Text as T
import           System.Console.Haskeline (MonadException)
import qualified System.Console.Haskeline as H

newtype InputT m a = InputT { unInputT :: H.InputT (ReaderT (Completions m) m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans InputT where
    lift = InputT . lift . lift

instance (MonadIO m) => MonadIO (InputT m) where
    liftIO = lift . liftIO

newtype Completions m = Completions { getCompletions :: H.CompletionFunc (ReaderT (Completions m) m) }

noCompletions :: (Monad m) => Completions m
noCompletions = Completions H.noCompletion

wordCompletions :: (Monad m) => [Char] -> [Text] -> Completions m
wordCompletions whitespace words = Completions $ H.completeWord Nothing whitespace go
    where
    go s = pure . fmap H.simpleCompletion
         . filter (s `isPrefixOf`)
         $ map T.unpack words
{-# ANN module ("HLint: ignore Use String" :: [Char]) #-}

getInputLine :: (Monad m, MonadException m) => Completions m -> Text -> InputT m (Maybe Text)
getInputLine compl prompt = InputT $
    (fmap.fmap) toS $ H.mapInputT (local (const compl)) (H.getInputLine (T.unpack prompt))

runInputT :: (MonadException m) => Maybe FilePath -> InputT m a -> m a
runInputT histfile = (`runReaderT` noCompletions)
                     . H.runInputT (H.setComplete dynamicCompletion H.defaultSettings { H.historyFile = histfile })
                     . unInputT
    where
    dynamicCompletion cx = do
        compl <- ask
        getCompletions compl cx

outputStrLn :: (MonadIO m) => Text -> InputT m ()
outputStrLn  = InputT . H.outputStrLn . T.unpack
