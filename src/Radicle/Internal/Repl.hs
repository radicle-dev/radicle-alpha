module Radicle.Internal.Repl where

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Safe (readMay)
import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, outputStrLn, runInputT)
import           System.Environment (lookupEnv)

import           Radicle.Internal.Chain
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty

repl :: IO ()
repl = do
    mcols <- liftIO (lookupEnv "COLUMNS")
    let cols = fromMaybe 80 $ mcols >>= readMay
        opts = AvailablePerLine cols 0.9
    pageWidthRef <- newIORef opts
    runCha
  where
    loop = do
      mi <- getInputLine "rad> "
      case mi of
        Nothing       -> return ()
        Just "(quit)" -> return ()
        Just inp      -> _ -- interpretOne (T.pack inp) >> loop

replSubs :: IORef PageWidth -> [Subscriber (InputT IO) ()]
replSubs pageWidthRef =
    [ subPageWidth pageWidthRef
    , subPrettyprint pageWidthRef
    ]

-- | Pretty print all values received
subPrettyprint :: IORef PageWidth -> Subscriber (InputT IO) ()
subPrettyprint pageWidthRef = makeSubscriber $ \v -> do
    pw <- liftIO $ readIORef pageWidthRef
    outputStrLn . T.unpack $ renderPretty pw v

-- | With each new value, check whether the number of columns has changed.
subPageWidth :: IORef PageWidth -> Subscriber (InputT IO) ()
subPageWidth pageWidthRef = makeSubscriber $ \_ -> liftIO $ do
    mcols <- lookupEnv "COLUMNS"
    case mcols >>= readMay of
      Nothing -> return ()
      Just cols -> writeIORef pageWidthRef $ AvailablePerLine cols 0.9

{-subReplLoop :: Subscriber (InputT IO) ()-}
{-subReplLoop = makeSubscriber $ \v -> case v of-}
    {-List [Atom "quit"] -> return ()-}
    {-_ -> -}
