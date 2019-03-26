module Radicle.Internal
    ( module X
    , createImpureBindings
    ) where

import           Protolude
import           Radicle.Daemon.Client (createDaemonClientPrimFns)
import           Radicle.Internal.Annotation as X
import           Radicle.Internal.CLI as X
import           Radicle.Internal.Core as X
import           Radicle.Internal.Effects as X
import           Radicle.Internal.Eval as X
import           Radicle.Internal.Identifier as X
import           Radicle.Internal.Interpret as X
import           Radicle.Internal.Json as X
import           Radicle.Internal.Parse as X
import           Radicle.Internal.Pretty as X
import           Radicle.Internal.PrimFns as X
import           Radicle.Internal.Type as X

-- | Create all impure bindings. This is in IO so as to create a
-- manager for the HTTP requests to the daemon.
createImpureBindings :: (MonadIO m, ReplM m) => [Text] -> IO (Bindings (PrimFns m))
createImpureBindings scriptArgs' = do
    daemonClientPrimFns <- createDaemonClientPrimFns
    pure $ addPrimFns (replPrimFns scriptArgs' <> daemonClientPrimFns) pureEnv
