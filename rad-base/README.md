# rad-base

The `rad-base` provides a home for the `radicle` prelude functionality, living under the
`rad` directory.

As well as this, there is one module `Radicle.Path` that exports `findModule`. This allows
`radicle-repl` to look up a module based on a path, using `rad` as a default.

```haskell
-- | Importing works as follows:
--
--    (1) If the RADPATH environment variable exists, search there
--    (2) Otherwise search in data-dir
--    (3) Finally we search in the current directory.
--
-- RADPATH should be a colon-separated list of directories (just like PATH).
findModule :: FilePath -> IO (Maybe FilePath)
findModule = ...
```
