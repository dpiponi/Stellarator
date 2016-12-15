{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Atari2600(MonadAtari(..),
                 hardware,
                 graphics,
                 stellaDebug,
                 debug,
                 clock,
                 regs,
                 pc,
                 p,
                 a,
                 x,
                 y,
                 s,
                 memory,
                 stellaClock,
                 sprites,
                 position,
                 stellaSDL,
                 pf,
                 oregisters,
                 iregisters,
                 trigger1,
                 Hardware(..),
                 Atari2600(..),
                 Registers(..),
                 intervalTimer) where

import Data.Word
import Data.Int
import Stella.Graphics
import Stella.Sprites
import Stella.TIARegisters
import Control.Lens
import Memory
import Control.Monad.State.Strict
import Stella.SDLState
import Stella.IntervalTimer
import Data.Array.IO
import DebugState

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

data Hardware = Hardware {
    _stellaDebug :: DebugState,
    _position :: (Int, Int),
    _stellaClock :: !Int64,
    _graphics :: Graphics,
    _sprites :: Sprites,
    _intervalTimer :: IntervalTimer,
    _trigger1 :: !Bool,
    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8,
    _stellaSDL :: SDLState,
    _pf :: Word64
}

$(makeLenses ''Hardware)

data Atari2600 = Atari2600 {
    _memory :: Memory,
    _hardware :: Hardware,
    _regs :: !Registers,
    _clock :: !Int64,
    _debug :: !Int
}

$(makeLenses ''Atari2600)
$(makeLenses ''Registers)

newtype MonadAtari a = M { unM :: StateT Atari2600 IO a }
    deriving (Functor, Applicative, Monad, MonadState Atari2600, MonadIO)
