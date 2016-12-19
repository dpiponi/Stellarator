{-# LANGUAGE TemplateHaskell #-}

module Stella.SDLState where

--import Control.Lens
import SDL.Vect
import SDL.Video
import Foreign.C.Types
import Control.Lens
import Metrics

{-
data SDLState = SDLState {
    _sdlBackSurface :: Surface,
    _sdlFrontSurface :: Surface,
    _sdlFrontWindow :: Window
}

$(makeLenses '' SDLState)

-}
