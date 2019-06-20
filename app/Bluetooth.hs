{-# Language LambdaCase #-}

module Bluetooth (export) where

import BasicPrelude

import System.Process (readProcess, readProcessWithExitCode, spawnProcess)
import System.Exit (ExitCode(..))
import Text.Regex.PCRE ((=~), getAllTextSubmatches, AllTextSubmatches)
import Options.Applicative
import qualified Data.Map.Strict as M

import Common

export :: ExportType
export = (M.singleton "bluetooth" (Client, execBluetooth), parseBluetooth)

parseBluetooth :: CommandParser
parseBluetooth = command "bluetooth" (info (pure "bluetooth") mempty)

execBluetooth :: Trigger -> IO Result
execBluetooth trigger = do
    enabled <- bluetoothEnabled
    when (trigger == MiddleMouse) toggleBluemanApplet
    when (trigger == RightMouse) (toggleBluetooth enabled)
    colour <- if not enabled then return "#FF0000"
        else do
            connected <- bluetoothConnected
            return $ if connected then "#00FF00" else "#FFA500"
    return $ resultPrintColourOkay "BT" colour


toggleBluetooth :: Bool -> IO ()
toggleBluetooth enabled = void $ readProcess "bluetoothctl" ["power", if enabled then "off" else "on"] ""

bluetoothEnabled :: IO Bool
bluetoothEnabled = (=~ pattern) <$> readProcess "bluetoothctl" ["show"] ""
    where pattern = "Powered: yes" :: String
bluetoothConnected :: IO Bool
bluetoothConnected = readProcessWithExitCode "bluetoothctl" ["info"] "" >>= \case
    (ExitSuccess, _, _) -> return True
    (ExitFailure _, _, _) -> return False

toggleBluemanApplet :: IO ()
toggleBluemanApplet = bluemanAppletRunning >>= bool killBluemanApplet startBluemanApplet

killBluemanApplet :: IO ()
killBluemanApplet = void $ readProcess "pkill" ["blueman-applet"] ""
startBluemanApplet :: IO ()
startBluemanApplet = void $ spawnProcess "blueman-applet" []
bluemanAppletRunning :: IO Bool
bluemanAppletRunning = readProcessWithExitCode "pgrep" ["blueman-applet"] "" >>= \case
    (ExitSuccess, _, _) -> return True
    (ExitFailure _, _, _) -> return False