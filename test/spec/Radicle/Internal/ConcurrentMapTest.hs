module Radicle.Internal.ConcurrentMapTest
    ( test_
    ) where

import           Protolude

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Radicle.Internal.ConcurrentMap as CMap


test_ :: TestTree
test_ = testGroup "Radicle.Internal.ConcurrentMap"

    [ testCase "modifyValue is atomic for same key" $ do
        cmap :: CMap.CMap () Int <- CMap.empty
        let concurrency = 100
        results <- replicateConcurrently concurrency $ CMap.modifyValue () cmap $ \case
            Nothing -> do
                -- We delay the thread so that an invocation of
                -- 'modifyValue' does not finish before the next one is
                -- started
                threadDelay (5 * 1000)
                pure (Just 1, 1)
            Just x -> do
                threadDelay (1 * 1000)
                pure (Just (x + 1), x + 1)

        sort results @?= [1..concurrency]

        finalValue <- CMap.lookup () cmap
        finalValue @?= Just concurrency


    -- Test that 'modifyValue' operations for different keys can be run
    -- concurrently. We check that by having all the invocations of the
    -- function passed to 'modifyValue' block until they have
    -- sychronized.
    , testCase "modifyValue no contention" $ do
        cmap :: CMap.CMap Int () <- CMap.empty

        insertsVar <- newTVarIO (0 :: Int)
        modificationsVar <- newTVarIO (0 :: Int)

        let concurrency = 100

        -- 'syncThreads tvar' blocks until 'concurrency' threads have
        -- entered the action 'syncThreads'
        let syncThreads tvar = do
                atomically $ modifyTVar' tvar (+ 1)
                -- This transaction blocks until the value in 'tvar' is
                -- @concurrency@
                atomically $ do
                    x <- readTVar tvar
                    check $ x == concurrency

        forConcurrently [1..concurrency] $ \key ->
            -- We run this twice to hit the 'Nothing' case and the
            -- 'Just' case
            replicateM_ 2 $
                CMap.modifyValue key cmap $ \case
                    Nothing -> do
                        syncThreads insertsVar
                        pure (Just (), ())
                    Just _ -> do
                        syncThreads modificationsVar
                        pure (Just (), ())

        inserts <- readTVarIO insertsVar
        inserts @?= concurrency

        modifications <- readTVarIO modificationsVar
        modifications @?= concurrency


    , testCase "modifyExistingValue is atomic for same key" $ do
        cmap :: CMap.CMap () Int <- CMap.empty
        CMap.insert () 0 cmap
        results <- replicateConcurrently 100 $ CMap.modifyExistingValue () cmap $ \x -> do
            threadDelay 1000
            pure (x + 1, x + 1)
        sort results @?= [ Just x | x <- [1..100] ]

        finalValue <- CMap.lookup () cmap
        finalValue @?= Just 100
    ]
