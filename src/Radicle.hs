{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}


-- | `radicle` - A LISP for blocktrees.
--
-- `radicle` is a *reflective* language, meaning evaluation can be modified,
-- thus redefining the language.
--
-- If you just want to use the language, you probably want the executables.
-- If however you want something more sophisticated - e.g., defining new
-- primops - you may need to use this as a library.  This is the only module
-- you should need to import.
module Radicle
    (
    -- * Language
    --
    -- | The definition of the core language.
    -- ** Evaluation
      interpret
    , interpretWithState
    , interpretMany
    , eval
    -- ** Datatypes
    --
    -- *** Value
    , ValueF(..)
    , LambdaArgs(..)
    , type Value
    , Type(..)
    , pattern Atom
    , pattern Keyword
    , pattern String
    , pattern Number
    , pattern Boolean
    , pattern List
    , pattern Vec
    , pattern PrimFn
    , pattern Dict
    , pattern Ref
    , pattern Lambda
    , maybeJson
    , UntaggedValue
    , untag
    -- *** LangError
    , LangError(..)
    , PatternMatchError(..)
    , ModuleError(..)
    , LangErrorData(..)
    , throwErrorHere
    , Reference(..)
    , Ident
    , mkIdent
    , unsafeToIdent
    , fromIdent
    , pattern Identifier
    , Env(..)
    , pureEnv
    , Lang
    , runLang
    , Bindings(..)
    , replBindings
    -- ** To/FromRadicle
    , ToRad(..)
    , FromRad(..)
    -- ** Re-exports
    , runIdentity

    -- * Parsing
    , parse
    , parseValues
    , parseREPL

    -- * Pretty-printing
    , renderPretty
    , renderPrettyDef
    , renderCompactPretty
    -- ** Re-exports
    , PageWidth(..)
    , Pretty

    -- * Repl
    , repl

    -- * Script
    , script

    -- * PrimFns
    , PrimFns(..)
    , purePrimFns
    , replPrimFns
    , addPrimFns
    , ReplM
    , createImpureBindings

    -- * CLI
    , getHistoryFile

    -- * Helpers
    , quote
    , ($$)
    , kwLookup
    , (??)
    , ignoreShebang

    ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Text.Prettyprint.Doc
import qualified Language.Haskell.TH.Syntax as TH
import           Protolude hiding (Type)
import           Radicle.Internal

compileTest
    :: Monad m
    => Text                 -- ^ Name of source file (for error reporting)
    -> Text                 -- ^ Source code to be interpreted
    -> m (Either (LangError Value) Value)
compileTest sourceName expr  =
  let bnds = do
        $(TH.lift (case runIdentity $ interpretWithState "[comp]" preludeSrc (replBindings []) of
        (Left e, _)  -> panic (show e)
        (Right _, r) -> removePrims r) )
  in interpret sourceName expr (bnds { bindingsPrimFns = bindingsPrimFns pureEnv })

