module Battery where

import BasicPrelude
import Data.Text (pack)
import System.Process (readProcess, shell)
import Text.Regex.PCRE ((=~), getAllTextSubmatches, AllTextSubmatches)

import Common

execBattery :: IO Result
execBattery = do
    output <- readProcess "acpi" ["-b"] ""
    let pattern = "Battery 0: (\\w+), (\\d+)%, (\\d\\d):(\\d\\d):(\\d\\d)" :: String
    return $ case map pack $ getAllTextSubmatches (output =~ pattern :: AllTextSubmatches [] String) of
        [_, status, percent, hours, minutes, seconds] ->
            makeResult status (read percent) (read hours) (read minutes) (read seconds)
        s -> resultPrintFailed (unlines s)

makeResult :: Text -> Int -> Int -> Int -> Int -> Result
makeResult status percent hours minutes seconds = defaultResult
    { text = prefix <> " " <> tshow percent <> "%"
    , foreColour = colour }
    where prefix = case status of
            "Discharging" -> "DIS"
            "Charging" -> "CHR"
            _ -> ""
          colour = case status of
            "Discharging"
                | percent < 20 -> Just "#FF0000"
                | percent < 40 -> Just "#FFAE00"
                | percent < 60 -> Just "#FFF600"
                | percent < 85 -> Just "#A8FF00"
                | otherwise    -> Nothing
            _ -> Nothing