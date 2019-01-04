module Benchmarks (main) where

import           Protolude

import           Criterion.Main

import           Radicle

steps :: [Text] -> Value
steps is = runIdentity $ snd <$> foldlM step (pureEnv, Number 0) is
  where
    step (s,_) t = do
      (res_, s') <- interpretWithState "[bench]" t s
      case res_ of
        Left e  -> panic (show e)
        Right v -> pure (s', v)

counter :: Int -> Rational
counter i = unwrap $ steps $
  [ "(def i (ref 0))"
  , "(def add (fn [x] (write-ref i (+ (read-ref i) x))))"
  , "(def eval (fn [expr state] (base-eval (list 'add expr) state)))" ]
  ++ (show <$> take i (cycle [1..20::Int]))
  where
    unwrap (Number r) = r
    unwrap _          = panic "expecting a number result!"

main :: IO ()
main = defaultMain [
  bgroup "counter" [ bench "1000"  $ whnf counter 1000
                   , bench "10000" $ whnf counter 10000
                   ]
    ]
