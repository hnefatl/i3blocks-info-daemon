{-# LANGUAGE MultiWayIf #-}

module Battery (export) where

import           BasicPrelude
import qualified Data.Map.Strict     as M
import           Data.Text           (pack)
import           Options.Applicative
import           System.Process      (readProcess, shell)
import           Text.Parsec

import           Common

export :: ExportType
export = (M.singleton "battery" (Client, execBattery), parseBattery)

parseBattery :: CommandParser
parseBattery = command "battery" (info (pure "battery") mempty)

type Time = (Int, Int, Int)
type ChargePercent = Int
data ChargingState = Charging ChargePercent Time
                   | Discharging ChargePercent Time
                   | Stable ChargePercent
                   deriving (Eq, Ord)

-- Handle output like
-- `Battery 0: Discharging, 95%, discharging at zero rate - will never fully discharge.`

parseChargingState :: Parsec Text () ChargingState
parseChargingState = string "Battery " >> digit >> string ": " >> choice [parseCharging, parseDischarging, parseStable]
    where parseCharging = string "Charging, " >> (Charging <$> (parsePercent <* string ", ") <*> parseTime)
          parseDischarging = string "Discharging, " >> (Discharging <$> (parsePercent <* string ", ") <*> parseTime)
          parseStable = string "Unknown, " >> (Stable <$> parsePercent)
          parsePercent = int <* char '%'
          parseTime = (,,) <$> (int <* char ':') <*> (int <* char ':') <*> int
          int = read . pack <$> many1 digit

showCharging :: ChargingState -> Result
showCharging (Charging pct (hours, minutes, _)) =
    resultPrintOkay $ "CHR " <> tshow pct <> "% (" <> tshow hours <> ":" <> tshow minutes <> ")"
showCharging (Discharging pct (hours, minutes, _)) = printWithColour text
    where text = "DIS " <> tshow pct <> "% (" <> tshow hours <> ":" <> tshow minutes <> ")"
          printWithColour = if
            | pct < 20 -> (`resultPrintColourOkay` "#FF0000")
            | pct < 40 -> (`resultPrintColourOkay` "#FFAE00")
            | pct < 60 -> (`resultPrintColourOkay` "#FFF600")
            | pct < 85 -> (`resultPrintColourOkay` "#A8FF00")
            | otherwise    -> resultPrintOkay
showCharging (Stable pct) = resultPrintOkay $ tshow pct <> "%"

execBattery :: Trigger -> IO Result
execBattery _ = do
    output <- pack <$> readProcess "acpi" ["-b"] ""
    return $ case parse parseChargingState "" output of
        Left err -> resultPrintFailed $ pack $ show err
        Right chargeState -> showCharging chargeState