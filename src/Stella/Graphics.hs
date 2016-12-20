{-# LANGUAGE TemplateHaskell #-}

module Stella.Graphics(Graphics(..), start, {- delayP0, delayP1, delayBall, -} oldGrp0, oldGrp1, newGrp0, newGrp1 {-, oldBall, newBall-}) where

import Control.Lens
import Data.Word

data Graphics = Graphics {
    -- _delayP0 :: !Bool,
    -- _delayP1 :: !Bool,
    -- _delayBall :: !Bool,
    _oldGrp0 :: !Word8,
    _newGrp0 :: !Word8,
    _oldGrp1 :: !Word8,
    _newGrp1 :: !Word8
    -- _oldBall :: !Bool,
    -- _newBall :: !Bool
}

$(makeLenses ''Graphics)

start :: Graphics
start = Graphics {
          _oldGrp0 = 0,
          _newGrp0 = 0,
          _oldGrp1 = 0,
          _newGrp1 = 0
          -- _oldBall = False,
          -- _newBall = False
      }
