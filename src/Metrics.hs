module Metrics where

import Data.Int

-- Some screen-related metrics
screenScanLines :: Int
screenScanLines = 192+10+10
picy :: Int
picy = 40-10
picx :: Int
picx = 68

screenWidth, screenHeight :: Int
--(screenWidth, screenHeight) = (160, screenScanLines)
screenWidth = 32
screenHeight = 16

fps :: Int64
fps = 60

{-
xscale, yscale :: Int
xscale = 5
yscale = 3
-}

{-
windowWidth, windowHeight :: Int
(windowWidth, windowHeight) = (xscale*screenWidth, yscale*screenHeight)
-}
