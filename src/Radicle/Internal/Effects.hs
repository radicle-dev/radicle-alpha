{-# OPTIONS_GHC -fno-warn-orphans #-}

module Radicle.Internal.Effects where

import           Protolude hiding (TypeError, toList)

import qualified Data.Map as Map
import qualified Data.Text as T
import           GHC.Exts (IsList(..))
import           System.Console.Haskeline
                 ( CompletionFunc
                 , InputT
                 , completeWord
                 , defaultSettings
                 , historyFile
                 , runInputT
                 , setComplete
                 , simpleCompletion
                 )

import           Radicle.Internal.Core
import           Radicle.Internal.Crypto
import           Radicle.Internal.Effects.Capabilities
import           Radicle.Internal.Eval
import           Radicle.Internal.Identifier (Ident(..), unsafeToIdent)
import           Radicle.Internal.Interpret
import           Radicle.Internal.Number (isInt)
import           Radicle.Internal.Pretty
import           Radicle.Internal.PrimFns
import           Radicle.Internal.Time as Time
import           Radicle.Internal.Type (Type(..))
import qualified Radicle.Internal.UUID as UUID

type ReplM m =
    ( Monad m, Stdout (Lang m), Stdin (Lang m)
    , MonadRandom m, UUID.MonadUUID m
    , CurrentTime m
    , ReadFile (Lang m)
    , System m
    , GetEnv (Lang m) Value
    , SetEnv (Lang m) Value
    , FileModule (Lang m))

instance MonadRandom (InputT IO) where
    getRandomBytes = liftIO . getRandomBytes
instance MonadRandom m => MonadRandom (LangT (Bindings (PrimFns m)) m) where
    getRandomBytes = lift . getRandomBytes

instance UUID.MonadUUID (InputT IO) where
    uuid = liftIO UUID.uuid
instance UUID.MonadUUID m => UUID.MonadUUID (LangT (Bindings (PrimFns m)) m) where
    uuid = lift UUID.uuid

repl :: Maybe FilePath -> Text -> Text -> Bindings (PrimFns (InputT IO)) -> IO ExitCode
repl histFile preFileName preCode bindings = do
    let settings = setComplete completion
                 $ defaultSettings { historyFile = histFile }
    r <- runInputT settings
        $ fmap fst $ runLang bindings
        $ void $ interpretMany preFileName preCode
    case r of
        Left (LangError _ (Exit n)) -> pure (exitCode n)
        Left e -> do putPrettyAnsi e
                     pure $ ExitFailure 1
        Right () -> pure ExitSuccess

exitCode :: Int -> ExitCode
exitCode 0 = ExitSuccess
exitCode n = ExitFailure n

completion :: Monad m => CompletionFunc m
completion = completeWord Nothing ['(', ')', ' ', '\n'] go
  where
    -- Any type for first param will do
    bnds :: Bindings (PrimFns (InputT IO))
    bnds = replBindings []

    go s = pure $ fmap simpleCompletion
         $ filter (s `isPrefixOf`)
         $ fmap (T.unpack . fromIdent)
         $ (Map.keys . fromEnv $ bindingsEnv bnds)
        <> Map.keys (getPrimFns $ bindingsPrimFns bnds)

replBindings :: ReplM m => [Text] -> Bindings (PrimFns m)
replBindings sysArgs = addPrimFns (replPrimFns sysArgs) pureEnv

replPrimFns :: ReplM m => [Text] -> PrimFns m
replPrimFns sysArgs = fromList $ allDocs $
    [ ( "get-args!"
      , "Returns the list of the command-line arguments the script was called with"
      , \case
          [] -> pure . Vec $ fromList (String <$> sysArgs)
          xs -> throwErrorHere $ WrongNumberOfArgs "get-args!" 0 (length xs)
      )
    , ("put-str!"
      , "Prints a string."
      , oneArg "put-str!" $ \case
        (String x) -> do
            putStrS x
            pure nil
        v -> throwErrorHere $ TypeError "put-str!" 0 TString v
      )
    -- Note to self: 'doc!' has to remain a primfn. Defining it as a lambda in
    -- radicle itself is futile but you are welcome to try.
    , ( "doc!"
      , "Prints the documentation attached to a value and returns `()`. To retrieve\
        \ the docstring as a value use `doc` instead."
      , oneArg "doc!" $ \case
          Atom i -> do
            d <- lookupAtomDoc i
            putStrS $ fromMaybe (missingDocMsg i) d
            pure nil
          v -> throwErrorHere $ TypeError "doc!" 0 TAtom v
      )

    -- , ( "apropos!"
    --   , "Prints documentation for all documented variables in scope."
    --   , noArg "apropos!" $ do
    --         env <- gets bindingsEnv
    --         let docs = [ i <> "\n" <> doc | (Ident i, Just doc, _) <- toList env ]
    --         putStrS (T.intercalate "\n\n" docs)
    --         pure nil
    --   )

    , ( "get-line!"
      , "Reads a single line of input and returns it as a string."
      , \case
          [] -> maybe nil toRad <$> getLineS
          xs -> throwErrorHere $ WrongNumberOfArgs "get-line!" 0 (length xs)
      )

    , ("subscribe-to!"
      , "Expects a dict `s` (representing a subscription) and a function `f`. The dict\
        \ `s` should have a function `getter` at the key `:getter`. This function is called\
        \ repeatedly (with no arguments), its result is then evaluated and passed to `f`."
      , \case
        [x, v] -> do
            e <- gets bindingsEnv
            case (x, v) of
                (Dict m, fn) -> case Map.lookup (Keyword $ unsafeToIdent "getter") m of
                    Nothing -> throwErrorHere
                        $ OtherError "subscribe-to!: Expected ':getter' key"
                    Just g -> loop go
                      where
                        loop action = do
                            st <- get
                            join $ (action >> pure (loop action)) `catchError`
                                (\err -> case err of
                                   LangError _ (Exit _) -> pure (pure nil)
                                   LangError _ (Impossible _) -> do
                                       putPrettyAnsi err
                                       throwError err
                                   _ -> do
                                       putPrettyAnsi err
                                       put st
                                       pure (loop action))
                        go = do
                            -- We need to evaluate the getter in the original
                            -- environment in which it was defined, but the
                            -- /result/ of the getter is then evaluated in the
                            -- current environment.
                            y <- callFn g []
                            case y of
                              Vec ys -> do
                                ys' <- traverse baseEval ys
                                -- The application of the subscriber function is
                                -- evaluated in the original environment.
                                void $ withEnv identity (const e) (callFn fn [Vec ys'])
                              _ -> throwErrorHere $ OtherError "Getter should return a vector of values"
                _  -> throwErrorHere $ TypeError "subscribe-to!" 0 TDict x
        xs  -> throwErrorHere $ WrongNumberOfArgs "subscribe-to!" 2 (length xs))
    , ( "read-file!"
      , "Reads the contents of a file and returns it as a string."
      , oneArg "read-file" $ \case
          String filename -> readFileS filename >>= \case
              Left err -> throwErrorHere . OtherError $ "Error reading file: " <> err
              Right text -> pure $ String text
          v -> throwErrorHere $ TypeError "read-file!" 0 TString v
      )
    , ( "open-file!"
      , "Open file in the specified mode (`:read`, `:write`, `:append`, `:read-write`)."
      , twoArg "open-file!" $ \case
          (String file, Keyword (Ident mode)) -> do
              mode' <- case mode of
                  "read" -> pure ReadMode
                  "write" -> pure WriteMode
                  "append" -> pure AppendMode
                  "read-write" -> pure ReadWriteMode
                  x -> throwErrorHere
                     . OtherError
                     $ "Expected one of :read, :write, :append, or :read-write."
                    <> "Got: " <> x
              res <- openFileS file mode'
              case res of
                  Left e  -> throwErrorHere . OtherError $ "open-file!:" <> e
                  Right v -> newHandle v
          (v, Keyword _) -> throwErrorHere $ TypeError "open-file!" 0 TString v
          (_, v) -> throwErrorHere $ TypeError "open-file!" 1 TKeyword v
      )
    , ( "load!"
      , "Evaluates the contents of a file. Each seperate radicle expression is\
        \ transacted according to the current definition of `tx`."
      , oneArg "load!" $ \case
          String filename -> readFileS filename >>= \case
              Left err -> throwErrorHere . OtherError $ "Error reading file: " <> err
              Right text -> interpretMany filename (ignoreShebang text)
          v -> throwErrorHere $ TypeError "load!" 0 TString v
      )
    , ( "gen-key-pair!"
      , "Given an elliptic curve, generates a cryptographic key-pair. Use\
        \ `default-ecc-curve` for a default value for the elliptic curve."
      , oneArg "gen-key-pair!" $ \case
          curvev -> do
            curve <- hoistEither . first (toLangError . OtherError) $ fromRad curvev
            (pk, sk) <- generateKeyPair curve
            let pkv = toRad pk
            let skv = toRad sk
            pure . Dict . Map.fromList $
              [ (Keyword (Ident "private-key"), skv)
              , (Keyword (Ident "public-key"), pkv)
              ]
      )
    , ( "gen-signature!"
      , "Given a private key and a message (a string), generates a cryptographic\
        \ signature for the message."
      , twoArg "gen-signature!" $ \case
          (skv, String msg) -> do
            sk <- hoistEither . first (toLangError . OtherError) $ fromRad skv
            toRad <$> signText sk msg
          (_, v) -> throwErrorHere $ TypeError "gen-signature!" 1 TString v
      )
    , ( "uuid!"
      , "Generates a random UUID."
      , \case
          [] -> String <$> UUID.uuid
          xs -> throwErrorHere $ WrongNumberOfArgs "uuid!" 0 (length xs)
      )
    , ( "system!"
      , "(system! proc) execute a system process. Returns the dict with the form\
        \ ```\
        \    { :stdin maybe-handle\
        \      :stdout maybe-handle\
        \      :stderr maybe-handle\
        \      :proc prochandle\
        \    }\
        \ ```\
        \ Where `maybe-handle` is either `[:just handle]` or `:nothing`.\
        \ Note that this is quite a low-level function; higher-level ones are more convenient."
      , oneArg "system!" $ \proc -> do
          proc' <- hoistEither $ first (toLangError . OtherError) $ fromRad proc
          res <- systemS proc'
          let ifJustCreate x = case x of
                Nothing -> pure Nothing
                Just v  -> Just <$> newHandle v
          case res of
              (a, b, c, ph) -> do
                  a' <- ifJustCreate a
                  b' <- ifJustCreate b
                  c' <- ifJustCreate c
                  ph' <- newProcessHandle ph
                  pure $ Dict $ Map.fromList $ first (Keyword . Ident) <$>
                    [ ("stdin", toRad a')
                    , ("stdout", toRad b')
                    , ("stderr", toRad c')
                    , ("proc", toRad ph')
                    ]
      )
    , ( "stdin!"
      , "A handle for standard in."
      , noArg "stdin!" $ pure $ Handle StdIn
      )
    , ( "stdout!"
      , "A handle for standard out."
      , noArg "stdout!" $ pure $ Handle StdIn
      )
    , ( "stderr!"
      , "A handle for standard error."
      , noArg "stderr!" $ pure $ Handle StdErr
      )
    , ( "write-handle!"
      , "Write a string to the provided handle."
      , twoArg "write-handle!" $ \case
          (Handle h, String msg) -> do
              h' <- lookupHandle h
              hPutStrS h' msg
              pure $ Keyword $ Ident "ok"
          (Handle _, v) -> throwErrorHere $ TypeError "write-handle!" 1 TString v
          (v, _) -> throwErrorHere $ TypeError "write-handle!" 0 THandle v
      )
    , ( "read-line-handle!"
      , "Read a single line from a handle. Returns the string read, or the\
        \ keyword `:eof` if an EOF is encountered."
      , oneArg "read-line-handle!" $ \case
          Handle h -> do
              h' <- lookupHandle h
              hGetLineS h' >>= \case
                  Nothing -> pure . Keyword $ Ident "eof"
                  Just v -> pure $ String v
          v -> throwErrorHere $ TypeError "read-line-handle!" 0 THandle v
      )
    , ( "wait-for-process!"
      , "Block until process terminates."
      , oneArg "wait-for-process!" $ \case
          ProcHandle h -> do
              h' <- lookupProcHandle h
              toRad <$> waitForProcessS h'
          v -> throwErrorHere $ TypeError "wait-for-process!" 0 TProcHandle v
      )
    , ( "close-handle!"
      , "Close a handle"
      , oneArg "close-handle!" $ \case
          Handle h -> do
              h' <- lookupHandle h
              toRad <$> hCloseS h'
          v -> throwErrorHere $ TypeError "wait-for-process!" 0 TProcHandle v
      )
    , ( "exit!"
      , "Exit the interpreter immediately with the given exit code."
      , oneArg "exit!" $ \case
          Number q -> case isInt q of
            Left _ -> throwErrorHere $ OtherError $ "exit!: number is not an integer"
            Right n -> throwErrorHere $ Exit n
          v -> throwErrorHere $ TypeError "exit!" 0 TNumber v
      )
    , ( "now!"
      , "Returns a timestamp for the current Coordinated Universal Time (UTC), right now, formatted according to ISO 8601."
      , \case
          [] -> do t <- currentTime
                   pure . String . Time.formatTime $ t
          xs -> throwErrorHere $ WrongNumberOfArgs "exit!" 0 (length xs)
      )
    , ( "load-ns!"
      -- TODO(james): update doc
      , "Given a file whose code starts with module metadata, creates the module.\
        \ That is, the file is evaluated as if the code was wrapped in `(module ...)`."
      , oneArg "load-ns!" $ \case
          String filename -> do
            ef <- fileModuleS filename
            file <- case ef of
                Nothing -> throwError . toLangError . OtherError $ "File not found in RADPATH: " <> filename
                Just f -> pure f
            t_ <- fmap ignoreShebang <$> readFileS file
            t <- hoistEither . first (toLangError . OtherError) $ t_
            interpretMany filename t
          v -> throwErrorHere $ TypeError "file-module!" 0 TString v
      )
    , ( "find-module-file!"
      , "Find a file according to radicle search path rules. These are: \
        \ 1) If RADPATH is set, first search there; \
        \ 2) If RADPATH is not set, search in the distribution directory \
        \ 3) If the file is still not found, search in the current directory."
      , oneArg "find-module-file!" $ \case
          String filename -> do
            t_ <- fileModuleS filename
            case t_ of
                Nothing -> throwError . toLangError . OtherError $ "File not found in RADPATH: " <> filename
                Just t  -> pure $ String t
          v -> throwErrorHere $ TypeError "find-module-file!" 0 TString v
      )
    , ( "cd!"
      , "Change the current working directory."
      , oneArg "cd!" $ \case
          String filename -> do
              setCurrentDirS $ toS filename
              pure $ Keyword $ Ident "ok"
          v -> throwErrorHere $ TypeError "cd!" 0 TString v
      )
    ]

-- | Since `#` is not a comment in radicle, we need to explictly remove shebang
-- lines.
ignoreShebang :: Text -> Text
ignoreShebang src = case T.lines src of
    f:rest -> T.unlines $ if "#!" `T.isPrefixOf` f then mempty:rest else f:rest
    _      -> src
