{-# LANGUAGE TemplateHaskell #-}

module Stella.SDLState where

--import Control.Lens
import SDL.Event
import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video
import SDL.Video.Renderer
import Foreign.C.Types

data SDLState = SDLState {
    _sdlBackSurface :: !Surface,
    _sdlFrontSurface :: !Surface,
    _sdlFrontWindow :: !Window
}

-- $(makeLenses '' SDLState)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, 192)

xscale, yscale :: CInt
xscale = 5
yscale = 3

renderDisplay :: SDLState -> IO ()
renderDisplay (SDLState back front window) = do
    unlockSurface back
    surfaceBlitScaled back Nothing front
                (Just (Rectangle (P (V2 0 0))
                                 (V2 (screenWidth*xscale) (screenHeight*yscale))))
    lockSurface back
    updateWindowSurface window
