module Radicle.Internal.Primops
  ( pureEnv
  , purePrimops
  ) where

import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.State
import           Data.Bifunctor (first)
import           Data.Foldable (foldlM, foldrM)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.Scientific (Scientific)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import           GHC.Exts (IsList(..))

import           Radicle.Internal.Core
import           Radicle.Internal.Pretty

-- | A Bindings with an Env containing only 'eval' and only pure primops.
pureEnv :: (Monad m) => Bindings m
pureEnv = Bindings e purePrimops mempty 0
  where
    e = fromList [(toIdent "eval", Primop $ toIdent "base-eval")]

-- | The universal primops. These are available in chain evaluation, and are
-- not shadowable via 'define'.
purePrimops :: forall m. (Monad m) => Primops m
purePrimops = Map.fromList $ first Ident <$>
    [ ("base-eval", evalArgs $ \args -> case args of
          [x] -> baseEval x
          xs  -> throwError $ WrongNumberOfArgs "base-eval" 1 (length xs))
    , ("list", evalArgs $ \args -> pure $ List args)
    , ("quote", \args -> case args of
          [v] -> pure v
          xs  -> throwError $ WrongNumberOfArgs "quote" 1 (length xs))
    , ("define", \args -> case args of
          [Atom name, val] -> do
              val' <- baseEval val
              defineAtom name val'
              pure nil
          [_, _]           -> throwError $ OtherError "define expects atom for first arg"
          xs               -> throwError $ WrongNumberOfArgs "define" 2 (length xs))
    , ("catch", \args -> case args of
          [l, form, handler] -> do
              mlabel <- baseEval l
              case mlabel of
                  Atom label -> baseEval form `catchError` \e -> do
                     (thrownLabel, thrownValue) <- errorToValue e
                     if thrownLabel == label || label == Ident "any"
                         then handler $$ [thrownValue]
                         else baseEval form
                  _ -> throwError $ TypeError "catch: first argument must be atom"
          xs -> throwError $ WrongNumberOfArgs "catch" 3 (length xs))
    , ("throw", evalArgs $ \args -> case args of
          [Atom label, exc] -> throwError $ ThrownError label exc
          [_, _]            -> throwError $ TypeError "throw: first argument must be atom"
          xs                -> throwError $ WrongNumberOfArgs "throw" 2 (length xs))
    , ("eq?", evalArgs $ \args -> case args of
          [a, b] -> pure $ Boolean (a == b)
          xs     -> throwError $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("cons", evalArgs $ \args -> case args of
          [x, List xs] -> pure $ List (x:xs)
          [_, _]       -> throwError $ TypeError "cons: second argument must be list"
          xs           -> throwError $ WrongNumberOfArgs "cons" 2 (length xs))
    , ("head", evalArgs $ \args -> case args of
          [List (x:_)] -> pure x
          [List []]    -> throwError $ OtherError "head: empty list"
          [_]          -> throwError $ TypeError "head: expects list argument"
          xs           -> throwError $ WrongNumberOfArgs "head" 1 (length xs))
    , ("tail", evalArgs $ \args -> case args of
          [List (_:xs)] -> pure $ List xs
          [List []]     -> throwError $ OtherError "tail: empty list"
          [_]           -> throwError $ TypeError "tail: expects list argument"
          xs            -> throwError $ WrongNumberOfArgs "tail" 1 (length xs))
    , ("lookup", evalArgs $ \args -> case args of
          [Atom a, Dict m] -> pure $ case Map.lookup a m of
              Just v  -> v
              -- Probably an exception is better, but that seems cruel
              -- when you have no exception handling facilities.
              Nothing -> nil
          [Atom _, _]           -> throwError
                                 $ TypeError "lookup: second argument must be map"
          [_, Dict _]      -> throwError
                                 $ TypeError "lookup: first argument must be atom"
          xs -> throwError $ WrongNumberOfArgs "lookup" 2 (length xs))
    , ("string-append", evalArgs $ \args ->
          let fromStr (String s) = Just s
              fromStr _          = Nothing
              ss = fromStr <$> args
          in if all isJust ss
              then pure . String . mconcat $ catMaybes ss
              else throwError $ TypeError "string-append: non-string argument")
    , ("insert", evalArgs $ \args -> case args of
          [Atom k, v, Dict m] -> pure . Dict $ Map.insert k v m
          [Atom _, _, _]           -> throwError
                                    $ TypeError "insert: third argument must be map"
          [_, _, _]                -> throwError
                                    $ TypeError "insert: first argument must be an atom"
          xs -> throwError $ WrongNumberOfArgs "insert" 3 (length xs))
    -- The semantics of + and - in Scheme is a little messed up. (+ 3)
    -- evaluates to 3, and of (- 3) to -3. That's pretty intuitive.
    -- But while (+ 3 2 1) evaluates to 6, (- 3 2 1) evaluates to 0. So with -
    -- it is *not* correct to say that it's a foldl (-) 0. Instead, it
    -- special-cases on one-argument application. (Similarly with * and /.)
    --
    -- In order to avoid this sort of thing, we don't allow +,*,- and / to be
    -- applied to a single argument.
    , numBinop (+) "+"
    , numBinop (*) "*"
    , numBinop (-) "-"
    , ("<", evalArgs $ \args -> case args of
          [Number x, Number y] -> pure $ Boolean (x < y)
          [_, _]               -> throwError $ TypeError "<: expecting number"
          xs                   -> throwError $ WrongNumberOfArgs "<" 2 (length xs))
    , (">", evalArgs $ \args -> case args of
          [Number x, Number y] -> pure $ Boolean (x > y)
          [_, _]               -> throwError $ TypeError ">: expecting number"
          xs                   -> throwError $ WrongNumberOfArgs ">" 2 (length xs))
    , ("foldl", evalArgs $ \args -> case args of
          [fn, init', List ls] -> foldlM (\b a -> (fn $$) [b,a]) init' ls
          [_, _, _]            -> throwError
                                $ TypeError "foldl: third argument should be a list"
          xs                   -> throwError $ WrongNumberOfArgs "foldl" 3 (length xs))
    , ("foldr", evalArgs $ \args -> case args of
          [fn, init', List ls] -> foldrM (\b a -> (fn $$) [b,a]) init' ls
          [_, _, _]            -> throwError
                                $ TypeError "foldr: third argument should be a list"
          xs                   -> throwError $ WrongNumberOfArgs "foldr" 3 (length xs))
    , ("map", evalArgs $ \args -> case args of
          [fn, List ls] -> List <$> traverse (fn $$) (pure <$> ls)
          [_, _]        -> throwError $ TypeError "map: second argument should be a list"
          xs            -> throwError $ WrongNumberOfArgs "map" 3 (length xs))
    , ("string?", evalArgs $ \args -> case args of
          [String _] -> pure $ Boolean True
          [_]        -> pure $ Boolean False
          xs         -> throwError $ WrongNumberOfArgs "string?" 1 (length xs))
    , ("boolean?", evalArgs $ \args -> case args of
          [Boolean _] -> pure $ Boolean True
          [_]         -> pure $ Boolean False
          xs          -> throwError $ WrongNumberOfArgs "boolean?" 1 (length xs))
    , ("number?", evalArgs $ \args -> case args of
          [Number _] -> pure $ Boolean True
          [_]        -> pure $ Boolean False
          xs         -> throwError $ WrongNumberOfArgs "number?" 1 (length xs))
    , ("member?", evalArgs $ \args -> case args of
          [x, List xs] -> pure . Boolean $ elem x xs
          [_, _]       -> throwError
                        $ TypeError "member?: second argument must be list"
          xs           -> throwError $ WrongNumberOfArgs "eq?" 2 (length xs))
    , ("if", \args -> case args of
          [cond, t, f] -> do
            b <- baseEval cond
            -- I hate this as much as everyone that might ever read Haskell, but
            -- in Lisps a lot of things that one might object to are True...
            if b == Boolean False then baseEval f else baseEval t
          xs -> throwError $ WrongNumberOfArgs "if" 3 (length xs))
    , ("read-ref", evalArgs $ \args -> case args of
          [Ref (Reference x)] -> gets bindingsRefs >>= \m -> case IntMap.lookup x m of
              Nothing -> throwError $ Impossible "undefined reference"
              Just v  -> pure v
          [_]                 -> throwError $ TypeError "read-ref: argument must be a ref"
          xs                  -> throwError $ WrongNumberOfArgs "read-ref" 1 (length xs))
    , ("write-ref", evalArgs $ \args -> case args of
          [Ref (Reference x), v] -> do
              st <- get
              put $ st { bindingsRefs = IntMap.insert x v $ bindingsRefs st }
              pure nil
          [_, _]                 -> throwError
                                  $ TypeError "write-ref: first argument must be a ref"
          xs                     -> throwError
                                  $ WrongNumberOfArgs "write-ref" 2 (length xs))
    , ( "show"
      , evalArgs $ \args -> case args of
          [x] -> pure (String (renderPrettyDef x))
          xs  -> throwError $ WrongNumberOfArgs "show" 1 (length xs)
      )
    ]
  where
    -- Many primops evaluate their arguments just as normal functions do.
    evalArgs f args = traverse baseEval args >>= f

    numBinop :: (Scientific -> Scientific -> Scientific)
             -> Text
             -> (Text, [Value Reference] -> Lang m (Value Reference))
    numBinop fn name = (name, evalArgs $ \args -> case args of
        Number x:x':xs -> foldM go (Number x) (x':xs)
          where
            go (Number a) (Number b) = pure . Number $ fn a b
            go _ _ = throwError . TypeError
                   $ name <> ": expecting number"
        [Number _] -> throwError
                    $ OtherError $ name <> ": expects at least 2 arguments"
        _ -> throwError $ TypeError $ name <> ": expecting number")
