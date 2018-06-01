-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities where

import           Control.Monad.State
import           Control.Monad.Except
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

-- | Like `runTestWith`, but uses the pureEnv
runTestWith' :: [Text] -> TestLang a -> (Either LangError a, [Text])
runTestWith' = runTestWith pureEnv


instance {-# OVERLAPPING #-} Stdin TestLang where
    getLineS = do
        ws <- lift get
        case worldStateStdin ws of
            [] -> throwError $ OtherError "test: out of stdin"
            h:hs -> lift (put $ ws { worldStateStdin = hs }) >> pure h

instance {-# OVERLAPPING #-} Stdout TestLang where
    putStrS t = lift $
        modify (\ws -> ws { worldStateStdout = t:worldStateStdout ws })
