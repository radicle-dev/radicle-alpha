module Main (main) where

import System.Environment
import qualified Data.Text.IO as T
import Control.Concurrent

import Radicle

main :: IO ()
main = do
    [file] <- getArgs
    src <- T.readFile file
    _ <- forkIO $ repl src
    threadDelay maxBound
