-- | This module defines instances for the classes in
-- Radicle.Internal.Subscriber.Capabilities that may be used for testing.
module Radicle.Internal.TestCapabilities where

import           Protolude

import           Data.Text (Text)

import           Radicle
import           Radicle.Internal.Subscriber.Capabilities

data WorldState = WorldState
    { worldStateStdin  :: [Text]
    , worldStateStdout :: [Text]
    , worldStateEnv    :: Env (Value Reference)
    }


type TestLang = Lang (State WorldState)

-- | Run a possibly side-effecting program with the given stdin input lines.
runTestWith
    :: Bindings (State WorldState)
    -> [Text]  -- The stdin (errors if it runs out)
    -> Text -- The program
    -> (Either (LangError (Value Reference)) (Value Reference), [Text])
runTestWith bindings inputs action =
    let ws = WorldState
            { worldStateStdin = inputs
            , worldStateStdout = []
            , worldStateEnv = bindingsEnv bindings
            }
    in case runState (fmap fst $ runLang bindings $ interpretMany "[test]" action) ws of
        (val, st) -> (val, reverse $ worldStateStdout st)

-- | Like `runTestWith`, but uses the pureEnv
runTestWith'
    :: [Text]
    -> Text
    -> (Either (LangError (Value Reference)) (Value Reference), [Text])
runTestWith' = runTestWith pureEnv

-- | Run a test without stdin/stdout
runTest
    :: Bindings (State WorldState)
    -> Text
    -> Either (LangError (Value Reference)) (Value Reference)
runTest bnds prog = fst $ runTestWith bnds [] prog

-- | Like 'runTest', but uses the pureEnv
runTest' :: Text -> Either (LangError (Value Reference)) (Value Reference)
runTest' = runTest pureEnv


instance {-# OVERLAPPING #-} Stdin TestLang where
    getLineS = do
        ws <- lift get
        case worldStateStdin ws of
            []   -> throwError $ OtherError "test: out of stdin"
            h:hs -> lift (put $ ws { worldStateStdin = hs }) >> pure h

instance {-# OVERLAPPING #-} Stdout TestLang where
    putStrS t = lift $
        modify (\ws -> ws { worldStateStdout = t:worldStateStdout ws })
