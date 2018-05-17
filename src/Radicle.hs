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

    -- * Subscribers
    , Subscriber(..)
    , makeSubscriber
    , addSubscriber
    , removeSubscribers

    -- * Parsing
    , parse
    , parseTest

    -- * Pretty-printing
    , renderPretty
    , renderPrettyDef
    , renderCompactPretty
    -- ** Re-exports
    , PageWidth(..)

    ) where

import           Data.Text.Prettyprint.Doc
import           Radicle.Internal
