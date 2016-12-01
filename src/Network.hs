module Network
  ( Frame(..)
  , network
  ) where

import Input
import Control.Varying

data Frame = Frame Int

tickDuration :: Float
tickDuration = 2 -- second

quitEvent :: Var Input (Event ())
quitEvent = use () $ onWhen (== InputQuit)

tickEvent :: Var Input (Event ())
tickEvent = var onTime >>> foldStream accrueTime (Nothing, 0) >>> var fst
  where onTime (InputTime t) = Just t
        onTime _             = Nothing
        accrueTime (_, t) dt
          | t + dt >= tickDuration = (Just (), t + dt - tickDuration)
          | otherwise = (Nothing, t + dt)

totalTicks :: Var Input Int
totalTicks = use 1 tickEvent >>> foldStream (+) 0

network :: Spline Input Frame ()
network = (Frame <$> totalTicks) `_untilEvent_` quitEvent
