module Radicle.Daemon.Error
    ( MachineError(..)
    , Error(..)
    , MonadError(..)
    , displayError
    ) where

import           Protolude

import           Radicle (LangError, Value, renderCompactPretty)
import           Radicle.Daemon.Ipfs
import           Radicle.Ipfs

-- | An error relating to a specific machine managed by the daemon.
data MachineError
  = InvalidInput (LangError Value)
  | IpfsError IpfsException
  | AckTimeout
  | DaemonError Text
  | MachineNotCached
  deriving (Show)

instance Exception MachineError

-- | An error that the daemon can encounter.
data Error
  = MachineError MachineId MachineError
  | CouldNotCreateMachine IpfsException
  | IpfsDaemonNotReachable
  deriving (Show)

instance Exception Error

displayError :: Error -> (Text, [(Text,Text)])
displayError = \case
  CouldNotCreateMachine (ipfsErr -> (msg, infos)) -> ("Could not create IPFS machine", ("message", msg) : infos)
  IpfsDaemonNotReachable -> ("Could not connect to Radicle IPFS daemon", [])
  MachineError id e -> let mid = ("machine-id", getMachineId id) in
    case e of
      InvalidInput err -> ("Invalid radicle input", [mid, ("radicle-error", renderCompactPretty err)])
      IpfsError (ipfsErr -> (msg, infos)) -> (msg, mid : infos)
      AckTimeout -> ("No response received from the machine owner. This could be because they are offline, or that waiting for the response timed out", [mid])
      MachineNotCached -> ("Machine was not found in cache", [mid])
      DaemonError err -> ("Internal error", [mid, ("error", err)])
  where
    ipfsErr = \case
      IpfsException msg -> ("There was an error using the IPFS daemon", [("ipfs-error", msg)])
      IpfsExceptionErrResp msg -> ("The IPFS daemon returned an error", [("ipfs-error", msg)])
      IpfsExceptionErrRespNoMsg -> ("There was an unknown error using IPFS", [])
      IpfsExceptionTimeout apiPath -> ("Timeout communicating with IPFS daemon", [("api-path", apiPath)])
      IpfsExceptionInvalidResponse url parseError -> ("Cannot parse IPFS daemon response", [("url", url), ("parse-error", parseError)])
      IpfsExceptionNoDaemon -> ("Cannot connect to the IPFS daemon", [])
      IpfsExceptionIpldParse addr parseError ->
            ("Failed to parse IPLD document ", [("addr", addressToText addr), ("parse-error", parseError)])
