-- | This executable generates the radicle reference docs. This is a markdown
-- document which contains documentation for all the primitive functions and all
-- the functions defined in the prelude.
module ReferenceDoc where

import           Protolude

import Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Radicle
import           System.Console.Haskeline (defaultSettings, runInputT)
import qualified GHC.Exts as GhcExts

main :: IO ()
main = do
    res_ <- runInputT defaultSettings $
             interpret
               "reference-doc"
               "(do (load! \"rad/prelude.rad\") (get-current-env))"
               replBindings
    res <- res_ `lPanic` "Error running the prelude."
    s :: Bindings () <- fromRad res `lPanic` "Couldn't convert radicle state."
    let e = Map.fromList
              [ (fromIdent i, d) | (i, Just d, _) <- GhcExts.toList (bindingsEnv s) ]
    checkAllDocumented e
    writeFile "docs/source/reference.md" (doc e)
  where
    doc e =
      let sec fns md = md <> "\n\n" <> funs e fns in
      T.intercalate "\n\n"
      [ sec basics "## Basics"
      , sec seqs "## Sequences"
      , sec lists "## Lists"
      , sec vecs "## Vectors"
      , sec dicts "## Dicts"
      , sec maths "## Maths"
      , sec structs "## Structures"
      , sec refs "## Refs"
      , sec evalFns "## Evaluation functions"
      , sec docs "## Documentation and testing"
      , sec envStuff "## Environment functions"
      , sec io "## Input/Output"
      , sec lens "## Lenses"
      , sec crypto "## Cryptography"
      , sec chainTools "## Chain tools" ]

    basics =
      [ "eq?", "not", "and", "or", "show", "string-append", "apply", "type", "atom?"
      , "boolean?", "string?", "number?", "keyword?", "list?", "dict?", "read", "throw"
      , "to-json", "uuid?" ]
    maths = ["+", "*", "-", "<", ">"]
    evalFns = ["base-eval", "eval"]
    envStuff = ["pure-env", "get-current-env", "set-current-env", "set-env!"]
    seqs = ["nth", "foldl", "foldr", "map", "seq"]
    lists =
      [ "list", "head", "tail", "empty?", "cons", "reverse", "length", "concat"
      , "filter", "range" ]
    vecs = ["<>", "add-left", "add-right"]
    dicts =
      ["dict", "lookup", "insert", "delete", "dict-from-list", "keys", "rekey"
      , "map-values"]
    structs = ["member?"]
    refs = ["ref", "read-ref", "write-ref", "modify-ref"]
    docs = ["doc", "doc!", "apropos!", "document", "should-be"]
    io =
      ["print!", "get-line!", "load!", "read-file!", "read-code!", "send-code!"
      , "send-prelude!", "subscribe-to!", "uuid!"]
    lens = ["@", "make-lens", "view", "view-ref", "set", "set-ref", "over", "over-ref", "id-lens", "..", "..."]
    crypto = ["verify-signature", "default-ecc-curve", "gen-key-pair!", "gen-signature!"]
    chainTools =
      [ "new-chain", "eval-in-chain", "update-chain", "add-quit", "add-send", "load-chain"
      , "pure-prelude-files", "pure-prelude-code", "store-exprs" ]

    allFns =
         basics ++ maths ++ evalFns ++ envStuff ++ seqs ++ lists ++ vecs
      ++ dicts ++ structs ++ refs ++ docs ++ io ++ lens ++ crypto ++ chainTools

    funs e fns = T.intercalate "\n\n\n" [ "`" <> f <> "`\n\n" <> getDoc e f | f <- fns ]

    getDoc e f = case Map.lookup f e of
      Just d -> d
      Nothing -> panic $ "While creating reference doc, couldn't find the docs for: " <> f

    checkAllDocumented e =
      let notDocumented = Map.keys e \\ allFns in
      if null notDocumented
        then pure ()
        else panic $ "The following functions need to be added to the reference doc: " <> T.intercalate ", " notDocumented

    lPanic (Left _)  m = panic m
    lPanic (Right r) _ = pure r
