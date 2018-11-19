-- | This executable generates the radicle reference docs. This is a markdown
-- document which contains documentation for all the primitive functions and all
-- the functions defined in the prelude.
module ReferenceDoc (main) where

import           Protolude

import qualified Data.Default as Default
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.Exts as GhcExts
import           Radicle
import qualified Radicle.Internal.Input as Input
import           Text.Pandoc

main :: IO ()
main = do
    res_ <- Input.runInputT Nothing $
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
    rst <- runPure (writeRST Default.def $ Pandoc nullMeta (doc e)) `lPanic` "Couldn't generate RST"
    writeFile "docs/source/reference.rst" rst
  where
    doc e =
      let sec tit fns p =
            [ Header 2 nullAttr (inlinePandoc tit)
            , Para (inlinePandoc p)
            ] ++ funs e fns in
      mconcat
      [ [ Header 1 nullAttr (inlinePandoc "Radicle Reference") ]
      , [ Para $ inlinePandoc $
             "These are the functions that are available in a new radicle chain after"
          <> " the prelude has been loaded."
        ]
      , sec "Basics" basics
            "Basic function used for checking equality, determining the type of a value, etc."
      , sec "Numerical functions" maths "Operations on numbers."
      , sec "Lists" lists $ typ "lists"
      , sec "Vectors" vecs $ typ "vectors"
      , sec "Sequences" seqs $ typ "boths lists and vectors"
      , sec "Dicts" dicts $ typ "dicts"
      , sec "Sets" sets $ typ "sets"
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
      , sec "Validation" validation
            "Functions for creating or combining *validators*, which are functions which return the\
            \ input unchanged or throw with an error message. These can be used for checking data before\
            \ accepting it onto a chain."
      , sec "Cryptography" crypto
            "Tools for creating and verifying cryptographic signatures, and generating private/public key pairs."
      , sec "Chain tools" chainTools
            "These functions can be used to simulate remote chains in the local REPL.\
            \ This is useful for experimenting with inputs or even new evaluation functions\
            \ before sending these to a remote chain."
      -- , sec "Issue chain" issueChain
      --       "These functions allow creating and interacting with the default issues chain."
      ]

    basics =
      [ "eq?", "not", "and", "or", "all", "some", "show", "string-append", "string-length"
      , "apply", "type", "atom?", "boolean?", "string?", "number?", "keyword?", "list?"
      , "dict?", "read", "read-many", "throw", "Y", "Y2", "to-json", "uuid?", "make-counter"
      , "public-key?" ]
    maths = ["+", "*", "-", "/", "<", ">", "integral?"]
    evalFns = ["base-eval", "eval", "updatable-eval"]
    envStuff = ["pure-env", "get-current-env", "set-current-env", "set-env!"]
    seqs = ["nth", "foldl", "foldr", "map", "seq", "take", "drop", "sort-by"]
    lists =
      [ "list", "nil", "head", "tail", "empty?", "cons", "reverse", "length", "concat"
      , "filter", "range", "list-with-head", "for-each" ]
    vecs = ["<>", "add-left", "add-right", "for-each-vec" ]
    dicts =
      ["dict", "lookup", "insert", "delete", "dict-from-list", "keys", "values", "rekey"
      , "map-values", "modify-map", "delete-many"]
    sets = ["set/empty", "set/insert", "set/delete", "set/member?", "set/delete", "set/from-seq", "set/to-vec"]
    structs = ["member?"]
    refs = ["ref", "read-ref", "write-ref", "modify-ref"]
    docs = ["help", "doc", "doc!", "apropos!", "document", "is-test-env"]
    io =
      [ "print!", "get-line!"
      , "load!", "read-file!", "read-code!", "send-code!"
      , "send-prelude!", "subscribe-to!", "uuid!", "read-line!", "exit!"
      , "now!"]
    lens = ["@", "@nth", "make-lens", "view", "view-ref", "set", "set-ref", "over", "over-ref", "id-lens", "..", "..."]
    validation =
      [ "validator/=", "validator/member", "validator/type", "validator/pred", "validator/every"
      , "validator/and", "validator/or", "validator/key", "validator/keys", "validator/uuid", "validator/signed"]
    crypto = ["verify-signature", "default-ecc-curve", "gen-key-pair!", "gen-signature!"]
    chainTools =
      [ "new-chain", "@var", "eval-in-chain", "enter-remote-chain", "update-chain", "add-quit", "add-send"
      , "load-chain", "pure-prelude-files", "pure-prelude-code", "store-exprs", "eval-fn-app"
      , "state-machine-eval", "state-machine-input", "state-machine-new-trans"
      , "state-machine-agree", "state-machine-disagree", "simple-trans", "update-chain-ref"]

    -- issueChain = ["create-issues-chain!", "list-issues", "new-issue!"]

    typ s = "Functions for manipulating " <> s <> "."

    -- Functions which shouldn't be in the reference docs.
    doNotInclude = ["head-shots", "get-head-shot", "eval__", "kim-trans", "pr-thread", "pr-trans", "_initial-prompt-text"]

    allFns =
         basics ++ maths ++ evalFns ++ envStuff ++ seqs ++ lists ++ vecs
      ++ dicts ++ sets ++ structs ++ refs ++ docs ++ io ++ lens ++ validation ++ crypto ++ chainTools
      -- ++ issueChain
      ++ doNotInclude

    -- Function symbol followed by a hard line break, followed by it's doc.
    funs e fns = mconcat
      [ [ Header 3 nullAttr [Code nullAttr (toS f) ]
        , Para (inlinePandoc (getDoc e f)) ]
      | f <- fns ]

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

    lPanic (Left e)  m = panic $ m <> ": " <> show e
    lPanic (Right r) _ = pure r

    inlinePandoc t = case runPure (readMarkdown Default.def t) of
      Right (Pandoc _ [Para is]) -> is
      Right _ -> panic $ "Expecting inline markdown but got blocks: " <> t
      Left err -> panic $ "Invalid markdown: " <> show err
