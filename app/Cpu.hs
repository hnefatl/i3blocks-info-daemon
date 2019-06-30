module Cpu (export) where

import BasicPrelude

import System.Process (readProcess, readProcessWithExitCode, spawnProcess)
import System.Statgrab (runStats, snapshot, CPU(..))
import Options.Applicative
import qualified Data.Map.Strict as M

import Common

export :: ExportType
export = (M.singleton "cpu" (Client, execCpu), parseCpu)

parseCpu :: CommandParser
parseCpu = command "cpu" (info (pure "cpu") mempty)

execCpu :: Trigger -> IO Result
execCpu _ = do
    cpuStats <- runStats snapshot
    return $ resultPrintOkay (tshow (cpuUser cpuStats) <> " " <> tshow (cpuTotal cpuStats))