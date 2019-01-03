module Benchmarks (main) where

import Protolude

import Criterion.Main

import Radicle

steps :: [Text] -> IO Value
steps is = snd <$> foldlM step (pureEnv, Number 0) is
  where
    step (s,_) t = do
      (res_, s') <- interpretWithState "[bench]" t s
      case res_ of
        Left e -> panic (show e)
        Right v -> pure (s', v)

counter :: Int -> IO Value
counter i = steps $
  [ "(def i (ref 0))"
  , "(def add (fn [x] (write-ref i (+ (read-ref i) x))))"
  , "(def eval (fn [expr state] (base-eval (list 'add expr) state)))" ]
  ++ (show <$> take i (cycle [1..20::Int]))

-- Our benchmark harness.
main :: IO ()
main = do
  v <- counter 100
  print v
  defaultMain [
    bgroup "counter" [ bench "10"  $ whnf counter 10
                   , bench "50"  $ whnf counter 50
                   , bench "1000"  $ whnf counter 1000
                   , bench "10000" $ whnf counter 10000
                   , bench "100000" $ whnf counter 100000
                   ]
    ]
