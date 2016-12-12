{-# LANGUAGE TemplateHaskell #-}

module Stella.SDLState where

--import Control.Lens
import SDL.Vect
import SDL.Video
import Control.Lens
import Metrics

data SDLState = SDLState {
    _sdlBackSurface :: !Surface,
    _sdlFrontSurface :: !Surface,
    _sdlFrontWindow :: !Window
}

$(makeLenses '' SDLState)

renderDisplay :: SDLState -> IO ()
renderDisplay (SDLState back front window) = do
    unlockSurface back
    surfaceBlitScaled back Nothing front
                (Just (Rectangle (P (V2 0 0))
                                 (V2 (screenWidth*xscale) (screenHeight*yscale))))
    lockSurface back
    --surfaceFillRect back Nothing (V4 0 0 0 0)
    updateWindowSurface window
