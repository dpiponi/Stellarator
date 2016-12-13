module Metrics where

import Foreign.C.Types

-- Some screen-related metrics
screenScanLines :: CInt
screenScanLines = 192+10+10
picy :: CInt
picy = 40-10
picx :: CInt
picx = 68

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, screenScanLines)

xscale, yscale :: CInt
xscale = 5
yscale = 3
