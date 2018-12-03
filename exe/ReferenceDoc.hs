-- | This executable generates the radicle reference docs. This is a markdown
-- document which contains documentation for all the primitive functions and all
-- the functions defined in the prelude.
module ReferenceDoc (main) where

import           Protolude

import qualified Data.Default as Default
import           Data.List hiding (map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC.Exts as GhcExts
import           Radicle
import           System.Console.Haskeline (defaultSettings, runInputT)
import           Text.Pandoc

main :: IO ()
main = do
    (_, s) <- runInputT defaultSettings $
             interpretWithState
               "reference-doc"
               "(load! \"rad/prelude.rad\")"
               replBindings
    let vars = GhcExts.toList (bindingsEnv s)
    let undocd = [ fromIdent iden | (iden, Nothing, _) <- vars ]
    checkAllDocumented undocd
    let e = Map.fromList [ (fromIdent iden, (docString, val)) | (iden, Just docString, val) <- vars ]
    checkAllInReference e
    rst <- runPure (writeRST Default.def $ Pandoc nullMeta (doc e)) `lPanic` "Couldn't generate RST"
    writeFile "docs/source/reference.rst" rst
  where
    doc e =
      let sec tit names p =
            [ Header 2 nullAttr (inlinePandoc tit)
            , Para (inlinePandoc p)
            ] ++ foldMap (valueDoc e) names in
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
      , sec "Strings" strings $ typ "strings"
      , sec "Structures" structs $ typ "lists, vectors and dicts"
      , sec "Patterns" patterns
            "Pattern matching is first-class in radicle so new patterns can easily be defined. These are the most essential."
      , sec "Refs" refs "Functions for creating, querying and modifying refs."
      , sec "Evaluation functions" evalFns "Utilities for creating and extending evaluation functions."
      , sec "Documentation and testing" docs
            "Functions for creating and querying documentation of variables in scope, and testing functions."
      , sec "Environment functions" envStuff "Utilities for modifying the current environment."
      , sec "Input/Output" io
            "Effectful functions. These functions are not available in 'pure' chains, but are available in the local REPL."
      , sec "Maybe" maybe'
            "Optionality is represented using `[:just x]` for when the value exists, and `:nothing` when it doesn't."
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
      ]

    basics =
      [ "eq?", "not", "and", "or", "all", "some", "show", "string-append", "string-length"
      , "apply", "type", "atom?", "boolean?", "string?", "number?", "keyword?", "vector?", "list?"
      , "dict?", "read", "read-many", "throw", "to-json", "uuid?", "make-counter"
      , "public-key?" ]
    maths = ["+", "*", "-", "/", "<", ">", "integral?"]
    evalFns = ["base-eval", "eval", "updatable-eval"]
    envStuff = ["pure-env", "get-current-env", "set-current-env", "set-env!"]
    seqs = ["empty-seq?", "nth", "foldl", "foldr", "map", "seq", "take", "drop", "sort-by", "zip"]
    lists =
      [ "list", "nil", "head", "tail", "empty?", "cons", "reverse", "length", "concat"
      , "filter", "range", "list-with-head" ]
    vecs = ["<>", "add-left", "add-right"]
    dicts =
      ["dict", "lookup", "insert", "delete", "dict-from-list", "keys", "values", "rekey"
      , "map-values", "map-keys", "modify-map", "delete-many", "exclusive-dict-merge"]
    sets = ["set/empty", "set/insert", "set/delete", "set/member?", "set/delete", "set/from-seq", "set/to-vec"]
    strings = ["intercalate", "unlines", "string-replace", "unwords"]
    structs = ["member?"]
    patterns = ["match-pat", "_", "/?", "/nil", "/cons", "/as", "non-linear-merge"]
    refs = ["ref", "read-ref", "write-ref", "modify-ref"]
    docs = ["help", "doc", "doc!", "apropos!", "is-test-env"]
    io =
      [ "print!", "get-line!"
      , "load!", "read-file!", "read-code!", "send-code!"
      , "put-str!", "process!", "shell!", "system!"
      , "send-prelude!", "subscribe-to!", "uuid!", "read-line!", "exit!"
      , "read-line-handle!", "wait-for-process!", "write-handle!", "now!"]
    maybe' = ["/Just", "maybe->>=", "maybe-foldlM"]
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
      ++ dicts ++ sets ++ strings ++ structs ++ patterns ++ refs ++ docs ++ io ++ maybe'
      ++ lens ++ validation ++ crypto ++ chainTools
      ++ doNotInclude

    valueDoc env name =
        case Map.lookup name env of
            Just (docString, Lambda args _ _) ->
                let callExample = "(" <> T.intercalate " " (name : map fromIdent args) <> ")"
                in [ Header 3 nullAttr [Code nullAttr (toS callExample) ]
                   , Para (inlinePandoc docString) ]
            Just (docString, _) ->
                [ Header 3 nullAttr [Code nullAttr (toS name) ]
                , Para (inlinePandoc docString) ]
            Nothing -> panic $ "While creating reference doc, couldn't find the docs for: " <> name


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
