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
      Value(..)
    , interpret
    , interpretMany
    , LangError(..)
    , ($$)
    , Ident
    , fromIdent
    , mkIdent
    , Env(..)
    , pureEmptyEnv
    , Lang
    , runLang
    , Bindings(..)
    , replBindings
    -- ** Re-exports
    , runIdentity

    -- * Parsing
    , parse
    , parseTest

    -- * Pretty-printing
    , renderPretty
    , renderPrettyDef
    , renderCompactPretty
    -- ** Re-exports
    , PageWidth(..)

    -- * Repl
    , repl
    ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Text.Prettyprint.Doc
import           Radicle.Internal

