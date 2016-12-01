module Input where

import Data.Maybe (mapMaybe)
import SDL

data Input = InputQuit
           | InputTime Float
           deriving (Show, Eq)

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

eventToInputMaybe :: Event -> Maybe Input
eventToInputMaybe ev
  | isQuit ev = Just InputQuit
  | otherwise = Nothing

eventsToInputs :: [Event] -> [Input]
eventsToInputs = mapMaybe eventToInputMaybe
