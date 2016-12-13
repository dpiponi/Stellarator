module Metrics where

-- Some screen-related metrics
screenScanLines :: Int
screenScanLines = 192+40+20
picy :: Int
picy = 40-40
picx :: Int
picx = 68

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = (160, screenScanLines)

xscale, yscale :: Int
xscale = 5
yscale = 3
