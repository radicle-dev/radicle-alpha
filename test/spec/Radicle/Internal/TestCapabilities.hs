{-# LANGUAGE TemplateHaskell #-}
-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities where

import           Control.Monad.State
import           Data.Text (Text)

import           Radicle
import           Radicle.Internal.Subscriber.Capabilities

data WorldState = WorldState
    { worldStateStdin  :: [Text]
    , worldStateStdout :: [Text]
    , worldStateEnv    :: Env
    }


type TestLang = Lang (State WorldState)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: Bindings (State WorldState)
    -> [Text]  -- The stdin (errors if it runs out)
    -> TestLang a
    -> (Either LangError a, [Text])
runTestWith bindings inputs action =
    let ws = WorldState
            { worldStateStdin = inputs
            , worldStateStdout = []
            , worldStateEnv = bindingsEnv bindings
            }
    in case runState (runLang bindings action) ws of
        (val, st) -> (val, reverse $ worldStateStdout st)

-- | Like `runTestWith`, but uses the pureEmptyEnv
runTestWith' :: [Text] -> TestLang a -> (Either LangError a, [Text])
runTestWith' = runTestWith pureEmptyEnv


instance Stdin (State WorldState) where
    getLineS = do
        ws <- get
        put $ ws { worldStateStdin = tail $ worldStateStdin ws }
        pure $ head $ worldStateStdin ws

instance Stdout (State WorldState) where
    putStrS t = modify (\ws -> ws { worldStateStdout = t:worldStateStdout ws })
