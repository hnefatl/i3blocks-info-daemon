{-# Language DeriveGeneric #-}
{-# Language LambdaCase #-}

module Main where

import BasicPrelude hiding (group)
import Data.Default (def)
import GHC.Generics (Generic)
import System.Daemon (DaemonOptions(..), PidFile(InHome), ensureDaemonRunning, runClient)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()

import Common (Result, Trigger, performResult, getTrigger)
import Battery (execBattery)

data Arg = Usage
         | Command Command
         deriving (Eq, Ord, Show)
data Command = Battery
             | CPU
             | Memory
             | Remote RemoteCommand
             deriving (Eq, Ord, Show)
data RemoteCommand = Stub
    deriving (Eq, Ord, Show, Generic)
instance Serialize RemoteCommand

port :: Int
port = 10059

parse :: [Text] -> Arg
parse ["--help"] = Usage
parse ["battery"] = Command Battery
parse ["cpu"] = Command CPU
parse ["memory"] = Command Memory
parse _ = Usage

usageText :: Text
usageText = "Usage: 'i3blocks-info-daemon [--help] <battery|cpu|memory>'"

main :: IO ()
main = do
    arg <- parse <$> getArgs
    trigger <- getTrigger
    case (arg, trigger) of
        (Usage, _) -> putStrLn usageText
        (Command c, Right t) -> do
            let options = DaemonOptions port InHome False
            ensureDaemonRunning "i3blocks-info-daemon" options daemonProcess
            performResult =<< handleCommand c t
        (_, Left triggerErr) -> putStrLn triggerErr >> exitWith (ExitFailure 1)

handleCommand :: Command -> Trigger -> IO Result
handleCommand Battery t = execBattery t
handleCommand (Remote c) trigger = runClient "localhost" port (c, trigger) >>= \case
    Nothing -> error ""
    Just t -> return t

daemonProcess :: (RemoteCommand, Trigger) -> IO Text
daemonProcess _ = undefined