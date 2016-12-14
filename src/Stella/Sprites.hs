{-# LANGUAGE TemplateHaskell #-}

module Stella.Sprites(Sprites(..), start, s_ppos0, s_ppos1, s_mpos0, s_mpos1, s_bpos) where

import Foreign.C.Types
import Control.Lens

data Sprites = Sprites {
    _s_ppos0 :: !Int,
    _s_ppos1 :: !Int,
    _s_mpos0 :: !Int,
    _s_mpos1 :: !Int,
    _s_bpos :: !Int
}

$(makeLenses ''Sprites)

start :: Sprites
start = Sprites {
          _s_ppos0 = 9999,
          _s_ppos1 = 9999,
          _s_mpos0 = 0,
          _s_mpos1 = 0,
          _s_bpos = 0
      }
