{-# Language DeriveGeneric #-}
{-# Language LambdaCase #-}

module Main where

import BasicPrelude hiding (group)
import Data.Default (def)
import GHC.Generics (Generic)
import System.Daemon (DaemonOptions(..), PidFile(InHome), ensureDaemonRunning, runClient)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()

import Common (Result, performResult)
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
    case arg of
        Usage -> putStrLn usageText
        Command c -> do
            let options = DaemonOptions port InHome False
            ensureDaemonRunning "i3blocks-info-daemon" options daemonProcess
            performResult =<< handleCommand c

handleCommand :: Command -> IO Result
handleCommand Battery = execBattery
handleCommand (Remote c) = runClient "localhost" port c >>= \case
    Nothing -> error ""
    Just t -> return t

daemonProcess :: RemoteCommand -> IO Text
daemonProcess _ = undefined