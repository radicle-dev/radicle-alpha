{-# LANGUAGE QuasiQuotes #-}
-- | This executable generates the radicle reference docs. This is a markdown
-- document which contains documentation for all the primitive functions and all
-- the functions defined in the prelude.
module ReferenceDoc where

import           Protolude

import           Data.List
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate
import qualified Data.Text as T
import qualified GHC.Exts as GhcExts
import           Radicle
import           Radicle.Internal.Doc (md)
import qualified Radicle.Internal.Doc as Doc
import           System.Console.Haskeline (defaultSettings, runInputT)

main :: IO ()
main = do
    res_ <- runInputT defaultSettings $
             interpret
               "reference-doc"
               "(do (load! \"rad/prelude.rad\") (get-current-env))"
               replBindings
    res <- res_ `lPanic` "Error running the prelude."
    s :: Bindings () <- fromRad res `lPanic` "Couldn't convert radicle state."
    let vars = GhcExts.toList (bindingsEnv s)
    let undocd = [ fromIdent iden | (iden, Nothing, _) <- vars ]
    checkAllDocumented undocd
    let e = Map.fromList [ (fromIdent iden, d) | (iden, Just d, _) <- vars ]
    checkAllInReference e
    d <- Doc.cleanupPandocMd (doc e) `lPanic` "Invalid markdown."
    writeFile "docs/source/reference.md" d
  where
    doc e =
      let sec (tit :: Text) fns p = toS [i|## #{tit}\n\n#{p}\n\n#{funs e fns}|] in
      T.intercalate "\n\n"
      [ "# Radicle reference"
      , "These are the functions that are available in a new radicle chain after the prelude has been loaded."
      , sec "Basics" basics
            "Basic function used for checking equality, determining the type of a value, etc."
      , sec "Numerical functions" maths "Operations on numbers."
      , sec "Lists" lists $ typ "lists"
      , sec "Vectors" vecs $ typ "vectors"
      , sec "Sequences" seqs $ typ "boths lists and vectors"
      , sec "Dicts" dicts $ typ "dicts"
      , sec "Structures" structs $ typ "lists, vectors and dicts"
      , sec "Refs" refs "Functions for creating, querying and modifying refs."
      , sec "Evaluation functions" evalFns "Utilities for creating and extending evaluation functions."
      , sec "Documentation and testing" docs
            "Functions for creating and querying documentation of variables in scope, and testing functions."
      , sec "Environment functions" envStuff "Utilities for modifying the current environment."
      , sec "Input/Output" io
            "Effectful functions. These functions are not available in 'pure' chains, but are available in the local REPL."
      , sec "Lenses" lens
            "Functional references into radicle values."
      , sec "Cryptography" crypto
            "Tools for creating and verifying cryptographic signatures, and generating private/public key pairs."
      , sec "Chain tools" chainTools
            [md|These functions can be used to simulate remote chains in the local REPL.
               This is useful for experimenting with inputs or even new evaluation functions"
               before sending these to a remote chain.|]
      ]

    basics =
      [ "eq?", "not", "and", "or", "all", "some", "show", "string-append", "apply", "type", "atom?"
      , "boolean?", "string?", "number?", "keyword?", "list?", "dict?", "read", "throw"
      , "Y", "Y2", "to-json", "uuid?", "make-counter" ]
    maths = ["+", "*", "-", "<", ">"]
    evalFns = ["base-eval", "eval", "updatable-eval"]
    envStuff = ["pure-env", "get-current-env", "set-current-env", "set-env!"]
    seqs = ["nth", "foldl", "foldr", "map", "seq"]
    lists =
      [ "list", "nil", "head", "tail", "empty?", "cons", "reverse", "length", "concat"
      , "filter", "range", "list-with-head" ]
    vecs = ["<>", "add-left", "add-right"]
    dicts =
      ["dict", "lookup", "insert", "delete", "dict-from-list", "keys", "rekey"
      , "map-values", "modify-map"]
    structs = ["member?"]
    refs = ["ref", "read-ref", "write-ref", "modify-ref"]
    docs = ["doc", "doc!", "apropos!", "document", "should-be"]
    io =
      ["print!", "get-line!", "load!", "read-file!", "read-code!", "send-code!"
      , "send-prelude!", "subscribe-to!", "uuid!", "read-line!"]
    lens = ["@", "make-lens", "view", "view-ref", "set", "set-ref", "over", "over-ref", "id-lens", "..", "..."]
    crypto = ["verify-signature", "default-ecc-curve", "gen-key-pair!", "gen-signature!"]
    chainTools =
      [ "new-chain", "eval-in-chain", "enter-remote-chain", "update-chain", "add-quit", "add-send"
      , "load-chain", "pure-prelude-files", "pure-prelude-code", "store-exprs", "eval-fn-app"
      , "state-machine-eval", "state-machine-input", "state-machine-new-trans", "state-machine-agree", "state-machine-disagree", "simple-trans"]

    typ s = "Functions for manipulating " <> s <> "."

    -- Functions which shouldn't be in the reference docs.
    doNotInclude = ["head-shots", "get-head-shot", "eval__", "kim-trans", "pr-thread", "pr-trans"]

    allFns =
         basics ++ maths ++ evalFns ++ envStuff ++ seqs ++ lists ++ vecs
      ++ dicts ++ structs ++ refs ++ docs ++ io ++ lens ++ crypto ++ chainTools
      ++ doNotInclude

    -- Function symbol followed by a hard line break, followed by it's doc.
    funs e fns = T.intercalate "\n\n\n" $ [ toS [i|`#{f}`  \n#{getDoc e f}|] | f <- fns ]

    getDoc e f = case Map.lookup f e of
      Just d -> d
      Nothing -> panic $ "While creating reference doc, couldn't find the docs for: " <> f

    checkAllDocumented = \case
      [] -> pure ()
      vs -> panic $ "The following functions have no documentation strings: " <> T.intercalate ", " vs

    checkAllInReference e =
      let notDocumented = Map.keys e \\ allFns in
      if null notDocumented
        then pure ()
        else panic $ "The following functions need to be added to the reference doc: " <> T.intercalate ", " notDocumented

    lPanic (Left _)  m = panic m
    lPanic (Right r) _ = pure r
