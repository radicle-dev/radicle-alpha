module Radicle.Daemon.Cli
    ( parse
    , Opts(..)
    ) where

import           Protolude hiding (option)

import           Options.Applicative

import qualified Radicle.Internal.CLI as Local

data Opts = Opts
  { port              :: Int
  , machineConfigFile :: FilePath
  , debug             :: Bool
  }

parse :: IO Opts
parse = execParser =<< parserInfo

parserInfo :: IO (ParserInfo Opts)
parserInfo = do
    defaultMachineConfigFile <- Local.getRadicleFile "machines.json"
    pure $
        info (opts defaultMachineConfigFile <**> helper)
            ( fullDesc
           <> progDesc "Run the radicle daemon"
           <> header "rad-daemon-radicle"
            )

opts :: FilePath -> Parser Opts
opts defaultMachineConfigFile = do
    port <-
        option auto
        ( long "port"
       <> help "daemon port"
       <> metavar "PORT"
       <> showDefault
       <> value 8909
        )
    machineConfigFile <-
        strOption
        ( long "machine-config"
       <> help "Where to store configuration for known machines"
       <> metavar "FILE"
       <> showDefault
       <> value defaultMachineConfigFile
        )
    debug <-
        switch
        ( long "debug"
       <> help "enable debug logging"
       <> showDefault
        )
    pure Opts{..}
