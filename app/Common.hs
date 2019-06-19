{-# Language DeriveGeneric #-}
{-# Language LambdaCase #-}

module Common where

import BasicPrelude
import GHC.Generics (Generic)
import Data.Functor ((<&>))
import Data.Text (pack)
import Data.Serialize (Serialize, put, get)
import Data.Serialize.Text ()
import System.Exit (ExitCode(..), exitWith)
import System.Environment (lookupEnv)

data Trigger = Timer
             | LeftMouse
             | MiddleMouse
             | RightMouse
             deriving (Eq, Ord, Show, Generic)
instance Serialize Trigger

getTrigger :: IO (Either Text Trigger)
getTrigger = lookupEnv "BLOCK_BUTTON" <&> \case
    Nothing -> Right Timer
    Just "1" -> Right LeftMouse
    Just "2" -> Right MiddleMouse
    Just "3" -> Right RightMouse
    Just s -> Left $ "Unknown value for environment variable BLOCK_BUTTON: " <> pack s

type Colour = Text

data Result = Result
    { text :: Text
    , foreColour :: Maybe Colour
    , backColour :: Maybe Colour
    , exitStatus :: ExitCode }
    deriving (Eq, Ord, Show)
instance Serialize Result where
    put r = do
        put $ text r
        put $ foreColour r
        put $ backColour r
        put $ case exitStatus r of
            ExitSuccess -> Nothing
            ExitFailure i -> Just i
    get = Result <$> get <*> get <*> get <*> getStatusCode
        where getStatusCode = get <&> \case
                Nothing -> ExitSuccess
                Just i -> ExitFailure i

showResult :: Result -> Text
showResult r = unlines $ [text r, text r] <> catMaybes [foreColour r, backColour r]
performResult :: Result -> IO ()
performResult r = putStrLn (showResult r) >> exitWith (exitStatus r)

defaultResult :: Result
defaultResult = Result
    { text = ""
    , foreColour = Nothing
    , backColour = Nothing
    , exitStatus = ExitSuccess }

resultPrintOkay :: Text -> Result
resultPrintOkay t = defaultResult { text = t }
resultPrintColourOkay :: Text -> Colour -> Result
resultPrintColourOkay t c = defaultResult { text = t, foreColour = Just c }
resultPrintFullColourOkay :: Text -> Colour -> Colour -> Result
resultPrintFullColourOkay t fore back = defaultResult { text = t, foreColour = Just fore, backColour = Just back }

resultPrintFailed :: Text -> Result
resultPrintFailed t = defaultResult { text = t, exitStatus = ExitFailure 1 }
resultPrintColourFailed :: Text -> Colour -> Result
resultPrintColourFailed t c = defaultResult { text = t, foreColour = Just c, exitStatus = ExitFailure 1 }
resultPrintFullColourFailed :: Text -> Colour -> Colour -> Result
resultPrintFullColourFailed t fore back = defaultResult { text = t, foreColour = Just fore, backColour = Just back, exitStatus = ExitFailure 1 }