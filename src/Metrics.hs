module Metrics where

-- Some screen-related metrics
screenScanLines :: Int
screenScanLines = 192+10+10
picy :: Int
picy = 40-10
picx :: Int
picx = 68

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (160, screenScanLines)

{-
xscale, yscale :: Int
xscale = 5
yscale = 3
-}

{-
windowWidth, windowHeight :: Int
(windowWidth, windowHeight) = (xscale*screenWidth, yscale*screenHeight)
-}
