{-# LANGUAGE TemplateHaskell #-}

module Stella.IntervalTimer where

import Data.Word
import Foreign.C.Types
import Control.Lens

{-
data IntervalTimer = IntervalTimer {
    _intim :: {-# UNPACK #-} !Word8,
    _subtimer :: {-# UNPACK #-} !Int,
    _interval :: {-# UNPACK #-} !Int
}

$(makeLenses ''IntervalTimer)
-}

{-
timerTick :: IntervalTimer -> IntervalTimer
timerTick (IntervalTimer 0      0         interval') = IntervalTimer (-1) (3*1-1) 1
timerTick (IntervalTimer intim' 0         interval') = IntervalTimer (intim'-1) (3*interval'-1) interval'
timerTick (IntervalTimer intim' subtimer' interval') = IntervalTimer intim' (subtimer'-1) interval'

start :: IntervalTimer
start = IntervalTimer {
    _intim = 0,
    _subtimer = 0,
    _interval = 0
}

start1 :: Word8 -> IntervalTimer
start1 v = IntervalTimer {
    _interval = 1,
    _subtimer = 1*3-1,
    _intim = v
}

start8 :: Word8 -> IntervalTimer
start8 v = IntervalTimer {
    _interval = 8,
    _subtimer = 8*3-1,
    _intim = v
}

start64 :: Word8 -> IntervalTimer
start64 v = IntervalTimer {
    _interval = 64,
    _subtimer = 64*3-1,
    _intim = v
}

start1024 :: Word8 -> IntervalTimer
start1024 v = IntervalTimer {
    _interval = 1024,
    _subtimer = 1024*3-1,
    _intim = v
}
-}
