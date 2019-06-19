{-# Language MultiWayIf #-}

module Battery where

import BasicPrelude
import Data.Text (pack)
import System.Process (readProcess, shell)
import Text.Regex.PCRE ((=~), getAllTextSubmatches, AllTextSubmatches)

import Common

execBattery :: Trigger -> IO Result
execBattery _ = do
    output <- readProcess "acpi" ["-b"] ""
    let pattern = "Battery 0: (\\w+), (\\d+)%, (\\d\\d):(\\d\\d):(\\d\\d)" :: String
    return $ case map pack $ getAllTextSubmatches (output =~ pattern :: AllTextSubmatches [] String) of
        [_, status, percent, hours, minutes, seconds] ->
            makeResult status (read percent) (read hours) (read minutes) (read seconds)
        s -> resultPrintFailed (unlines s)

makeResult :: Text -> Int -> Int -> Int -> Int -> Result
makeResult "Charging" percent hours minutes seconds = resultPrintOkay $ "CHR " <> tshow percent <> "%"
makeResult "Discharging" percent hours minutes seconds = defaultResult
    { text = "DIS " <> tshow percent <> "%"
    , foreColour = colour }
    where colour = if
            | percent < 20 -> Just "#FF0000"
            | percent < 40 -> Just "#FFAE00"
            | percent < 60 -> Just "#FFF600"
            | percent < 85 -> Just "#A8FF00"
            | otherwise    -> Nothing
makeResult _ percent hours minutes seconds = resultPrintOkay $ tshow percent <> "%"