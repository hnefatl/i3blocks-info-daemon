{-# LANGUAGE LambdaCase #-}

module Main where

import           BasicPrelude        hiding (group)
import           Data.Default        (def)
import qualified Data.Map.Strict     as M
import           Data.Serialize      (Serialize)
import           Data.Serialize.Text ()
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Daemon       (DaemonOptions (..), PidFile (InHome),
                                      ensureDaemonRunning, runClient)
import           System.Exit         (ExitCode (ExitFailure), exitWith)

import qualified Battery
import qualified Bluetooth
import           Common              (Command, CommandParser, ExportType,
                                      Result, RunType (..), Trigger, getTrigger,
                                      performResult, resultPrintFailed)

commands :: [ExportType]
commands = [ Bluetooth.export
           , Battery.export ]

handlers :: M.Map Command (RunType, Trigger -> IO Result)
handlers = M.unions $ map fst commands

port :: Int
port = 10059

parse :: IO Command
parse = execParser $ info (parser <**> helper) (progDesc desc)
    where parser = subparser $ mconcat $ map snd commands
          desc = "Compute block text for i3blocks."

main :: IO ()
main = do
    command <- parse
    trigger <- getTrigger
    case trigger of
        Right t -> do
            let options = DaemonOptions port InHome False
            ensureDaemonRunning "i3blocks-info-daemon" options daemonProcess
            performResult =<< handleCommand command t
        Left triggerErr -> putStrLn triggerErr >> exitWith (ExitFailure 48)

handleCommand :: Command -> Trigger -> IO Result
handleCommand c t = case M.lookup c handlers of
    Just (Client, handler) -> handler t
    Just (Daemon, _) -> runClient "localhost" port (c, t) >>= \case
        Nothing -> putStrLn "No response from daemon" >> exitWith (ExitFailure 47)
        Just result -> return result
    Nothing -> (putStrLn $ "No handler found for command '" <> c <> "'") >> exitWith (ExitFailure 47)

daemonProcess :: (Command, Trigger) -> IO Result
daemonProcess (c, t) = case M.lookup c handlers of
    Just (_, handler) -> handler t
    Nothing -> return $ resultPrintFailed $ "No handler found on daemon for '" <> c <>  "'"
