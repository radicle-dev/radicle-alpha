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
    , worldStateEnv    :: forall s. Env (Value (Reference s))
    }


type TestLang s = Lang s (State WorldState )

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: (forall s. Bindings s (State WorldState))
    -> [Text]  -- The stdin (errors if it runs out)
    -> Text -- The program
    -> (Either LangError (Value Int), [Text])
runTestWith bindings inputs action =
    let ws = WorldState
            { worldStateStdin = inputs
            , worldStateStdout = []
            , worldStateEnv = bindingsEnv bindings
            }
    in case runState (runLang bindings $ labelRefs <$> interpretMany "[test]" action) ws of
        (val, st) -> (val, reverse $ worldStateStdout st)

-- | Like `runTestWith`, but uses the pureEnv
runTestWith'
    :: [Text]
    -> Text
    -> (Either LangError (Value Int), [Text])
runTestWith' = runTestWith pureEnv

-- | Run a test without stdin/stdout
runTest
    :: (forall s. Bindings s (State WorldState))
    -> Text
    -> Either LangError (Value Int)
runTest bnds prog = fst $ runTestWith bnds [] prog

-- | Like 'runTest', but uses the pureEnv
runTest' :: Text -> Either LangError (Value Int)
runTest' = runTest pureEnv


instance {-# OVERLAPPING #-} Stdin (TestLang s) where
    getLineS = do
        ws <- lift get
        case worldStateStdin ws of
            [] -> throwError $ OtherError "test: out of stdin"
            h:hs -> lift (put $ ws { worldStateStdin = hs }) >> pure h

instance {-# OVERLAPPING #-} Stdout (TestLang s) where
    putStrS t = lift $
        modify (\ws -> ws { worldStateStdout = t:worldStateStdout ws })
