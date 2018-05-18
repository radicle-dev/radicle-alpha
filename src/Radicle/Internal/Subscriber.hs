{-# LANGUAGE ScopedTypeVariables #-}
module Radicle.Internal.Subscriber where

import           Control.Lens (at, (&), (<>~), (?~), (^.))
import           Control.Monad.Except
import           Control.Monad.State (gets)
import           Data.Bifunctor (first)
import           Data.Either
import qualified Data.Map as Map
import           Data.Text (Text)
import           System.Console.Haskeline

import           Radicle.Internal.Core
import           Radicle.Internal.Parse
import           Radicle.Internal.Pretty
import           Radicle.Internal.Subscriber.Capabilities

type ReplM m = (Stdout m, Stdin m, GetEnv (Lang m), SetEnv (Lang m))

repl :: Text -> IO ()
repl preCode = do
    r <- runInputT defaultSettings
        $ runLangT replBindings
        $ interpretMany "[pre]" preCode
    print r

replBindings :: ReplM m => Bindings m
replBindings = pureEmptyEnv & primops <>~ replPrimops

replPrimops :: forall m. ReplM m => Prims m
replPrimops = Map.fromList $ first identFromString <$>
    [ ("print!", \args -> case args of
        [x] -> do
            v <- eval x
            putStrS (renderPrettyDef v)
            pure v
        xs  -> throwError $ WrongNumberOfArgs "print!" 1 (length xs))

    , ("set-env!", \args -> case args of
        [Atom x, v] -> do
            v' <- eval v
            modifyEnvS (\e -> e & at x ?~ v')
            pure nil
        [_, _] -> throwError $ TypeError "Expected atom as first arg"
        xs  -> throwError $ WrongNumberOfArgs "set-env!" 2 (length xs))

    , ("get-line!", \args -> case args of
        [] -> do
            ln <- getLineS
            allPrims <- gets (^. primops)
            let p = parseValues "[stdin]" ln (Map.keys allPrims)
            case partitionEithers p of
                ([], results) -> pure $ List results
                (e:_, _)      -> throwError $ ParseError e
        xs  -> throwError $ WrongNumberOfArgs "get-line!" 0 (length xs))

    , ("subscribe-to!", \args -> case args of
        [x, v] -> do
            x' <- eval x
            v' <- eval v
            case (x', v') of
                (SortedMap m, fn) -> case m ^. at (identFromString "getter") of
                    Nothing -> throwError
                        $ OtherError "subscribe-to!: Expected 'getter' key"
                    Just g -> go
                      where
                        go = do
                            newBlock <- eval g
                            _ <- eval (fn $$ [newBlock])
                            go
                _  -> throwError $ TypeError "subscribe-to!: Expected sorted-map"
        xs  -> throwError $ WrongNumberOfArgs "subscribe-to!" 2 (length xs))
    ]
