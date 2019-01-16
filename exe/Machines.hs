-- | CLI for managing Radicle machines on IPFS
module Machines
    ( main
    ) where

import           Protolude hiding (option)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Options.Applicative
import           Radicle.Internal.MachineBackend.Ipfs

data Command = CommandCreate
    deriving (Show, Eq)

commandParser :: Parser Command
commandParser = subparser $
    command "create"
        (info (pure CommandCreate <**> helper)
        (progDesc "Create a Radicle machine and output its ID" ))

programParserInfo :: ParserInfo Command
programParserInfo =
    info (commandParser <**> helper) (progDesc "Manage Radicle machines on IPFS")

runCommand :: Command -> IO ()
runCommand CommandCreate = do
    label <- UUID.toText <$> UUID.nextRandom
    result <- ipfsMachineCreate label
    case result of
        Left err        -> die (toS err)
        Right machineId -> putStrLn machineId

main :: IO ()
main = do
    cmd <- execParser programParserInfo
    runCommand cmd
