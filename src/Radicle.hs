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
    , interpretMany
    , eval
    -- ** Datatypes
    --
    -- *** Value
    , Value(..)
    , maybeJson
    -- *** LangError
    , LangError(..)
    , Ident(..)
    , Reference(..)
    , mkIdent
    , Env(..)
    , pureEnv
    , Lang
    , runLang
    , Bindings(..)
    , replBindings
    -- ** To/FromRadicle
    , ToRadicle(..)
    , FromRadicle(..)
    -- ** Re-exports
    , runIdentity

    -- * Parsing
    , parse

    -- * Pretty-printing
    , renderPretty
    , renderPrettyDef
    , renderCompactPretty
    -- ** Re-exports
    , PageWidth(..)
    , Pretty

    -- * Repl
    , repl

    -- * Primops
    , Primops(..)
    , purePrimops
    , replPrimops
    , ReplM
    -- ** Primop helpers
    , evalArgs

    -- * CLI
    , getConfig
    , getHistory

    -- * Helpers
    , quote
    , ($$)
    , kwLookup
    , (??)

    ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Text.Prettyprint.Doc
import           Radicle.Internal
