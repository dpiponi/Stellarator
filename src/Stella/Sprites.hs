{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stella.Sprites where

import Foreign.C.Types
import Control.Lens
import Data.Array.IO
import Data.Array.Unboxed

{-
data Sprites = Sprites {
    _s_ppos0 :: !CInt,
    _s_ppos1 :: !CInt,
    _s_mpos0 :: !CInt,
    _s_mpos1 :: !CInt,
    _s_bpos :: !CInt
}

$(makeLenses ''Sprites)
-}

type Sprites = IOUArray SpriteCounter Int

newtype SpriteCounter = SpriteCounter Int deriving (Ord, Ix, Eq, Num)
pos_p0, pos_p1, pos_m0, pos_m1, pos_b :: SpriteCounter
pos_p0 = 0
pos_p1 = 1
pos_m0 = 2
pos_m1 = 3
pos_b = 4

start :: IO Sprites
start = do
    arr <- newListArray (pos_p0, pos_b) [0, 0, 9999, 9999, 0] :: IO (IOUArray SpriteCounter Int)
    return arr
