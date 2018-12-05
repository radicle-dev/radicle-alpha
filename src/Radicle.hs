{-# LANGUAGE PatternSynonyms #-}

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

    -- * Pretty-printing
    , renderPretty
    , renderPrettyDef
    , renderCompactPretty
    -- ** Re-exports
    , PageWidth(..)
    , Pretty

    -- * Repl
    , repl

    -- * PrimFns
    , PrimFns(..)
    , purePrimFns
    , replPrimFns
    , addPrimFns
    , ReplM

    -- * CLI
    , getHistoryFile

    -- * Helpers
    , quote
    , ($$)
    , kwLookup
    , (??)

    ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Text.Prettyprint.Doc
import           Radicle.Internal
