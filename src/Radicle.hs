-- | radicle - A LISP for blocktrees.
--
-- This is the only module you should need to import.
module Radicle
    (
    -- * Language
    --
    -- | The definition of the core language.
      Value(..)
    , interpret
    , LangError(..)
    , ($$)
    , Ident
    , fromIdent
    , makeIdent
    , Env(..)
    , primops

    -- * Chains
    , Chain(..)
    , genesisChain
    , foldChain
    , foldChainFromSrc
    -- ** Chain lenses
    , name
    , step
    , env
    , updateEnv
    , subscribers

    -- * Subscribers
    , Subscriber(..)
    , makeSubscriber

    -- * Parsing
    , parse
    , parseTest

    -- * Pretty-printing
    , renderPretty
    , renderPrettyDef
    , renderCompactPretty
    -- ** Re-exports
    , PageWidth(..)

    -- * REPL
    , repl
    ) where

import           Data.Text.Prettyprint.Doc
import           Radicle.Internal
