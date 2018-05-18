{-# LANGUAGE TemplateHaskell #-}
-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities where

import Control.Lens
import Control.Monad.State
import Data.Text (Text)

import Radicle
import Radicle.Internal.Subscriber.Capabilities

data WorldState = WorldState
    { _worldStateStdin :: [Text]
    , _worldStateStdout :: [Text]
    , _worldStateEnv :: Env
    }

makeFields ''WorldState

type TestLang = Lang (State WorldState)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: Bindings (State WorldState)
    -> [Text]  -- The stdin (errors if it runs out)
    -> TestLang a
    -> (Either LangError a, [Text])
runTestWith bindings inputs action =
    let ws = WorldState
            { _worldStateStdin = inputs
            , _worldStateStdout = []
            , _worldStateEnv = bindings ^. env
            }
    in case runState (runLangT bindings action) ws of
        (val, st) -> (val, reverse $ st ^. stdout)

-- | Like `runTestWith`, but uses the pureEmptyEnv
runTestWith' :: [Text] -> TestLang a -> (Either LangError a, [Text])
runTestWith' = runTestWith pureEmptyEnv


instance Stdin (State WorldState) where
    getLineS = head <$> (stdin <<%= tail)

instance Stdout (State WorldState) where
    putStrS t = stdout %= (t:)
