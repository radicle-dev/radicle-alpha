-- | CLI for managing Radicle machines on IPFS
module Machines
    ( main
    ) where

import           Protolude hiding (option)

import           Options.Applicative
import qualified Radicle.Daemon.Client as Client

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
    Client.MachineId machineId <- Client.newMachine
    putStrLn machineId

main :: IO ()
main = do
    cmd <- execParser programParserInfo
    runCommand cmd
