module Battery where

import BasicPrelude
import Data.Text (pack)
import System.Process (readProcess, shell)
import Text.Regex.PCRE ((=~), getAllTextSubmatches, AllTextSubmatches)

execBattery :: IO Text
execBattery = do
    output <- readProcess "acpi" ["-b"] ""
    let pattern = "Battery 0: (\\w+), (\\d+)%, (\\d\\d):(\\d\\d):(\\d\\d)" :: String
    case getAllTextSubmatches (output =~ pattern :: AllTextSubmatches [] String) of
        [_, status, percent, hours, minutes, seconds] -> return $ pack percent ++ "%"
        s -> error (show s)