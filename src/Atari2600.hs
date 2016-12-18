{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Atari2600(MonadAtari(..),
                 hardware,
                 graphics,
                 useHardware,
                 putHardware,
                 modifyHardware,
                 useMemory,
                 putMemory,
                 modifyMemory,
                 useRegisters,
                 putRegisters,
                 modifyRegisters,
                 useSprites,
                 putSprites,
                 modifySprites,
                 useClock,
                 putClock,
                 modifyClock,
                 useStellaClock,
                 putStellaClock,
                 modifyStellaClock,
                 useIntervalTimer,
                 putIntervalTimer,
                 modifyIntervalTimer,
                 useGraphics,
                 putGraphics,
                 modifyGraphics,
                 getORegisters,
                 getIRegisters,
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
import Data.IORef
import Stella.TIARegisters
import Control.Lens
import Control.Monad.Reader
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
    _position :: !(Int, Int),
    _trigger1 :: !Bool,
    _stellaSDL :: SDLState,
    _pf :: !Word64
}

$(makeLenses ''Hardware)

data Atari2600 = Atari2600 {
    _memory :: IORef Memory,
    _hardware :: IORef Hardware,
    _regs :: IORef Registers,
    _clock :: IORef Int64,
    _debug :: IORef Int,
    _sprites :: IORef Sprites,
    _intervalTimer :: IORef IntervalTimer,
    _graphics :: IORef Graphics,
    _stellaClock :: IORef Int64,
    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8
}

$(makeLenses ''Atari2600)
$(makeLenses ''Registers)

newtype MonadAtari a = M { unM :: ReaderT Atari2600 IO a }
      deriving (Functor, Applicative, Monad, MonadReader Atari2600, MonadIO)

{-# INLINE useHardware #-}
useHardware :: Getting b Hardware b -> MonadAtari b
useHardware lens = do
    atari <- ask
    hardware' <- liftIO $ readIORef (atari ^. hardware)
    return $! hardware' ^. lens

{-# INLINE putHardware #-}
putHardware :: ASetter Hardware Hardware a a -> a -> MonadAtari ()
putHardware lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. hardware) (set lens value)

{-# INLINE modifyHardware #-}
modifyHardware lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. hardware) (over lens modifier)

{-# INLINE useMemory #-}
useMemory :: Getting b Memory b -> MonadAtari b
useMemory lens = do
    atari <- ask
    memory' <- liftIO $ readIORef (atari ^. memory)
    return $! memory' ^. lens

{-# INLINE putMemory #-}
putMemory :: ASetter Memory Memory a a -> a -> MonadAtari ()
putMemory lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. memory) (set lens value)

{-# INLINE modifyMemory #-}
modifyMemory lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. memory) (over lens modifier)

{-# INLINE useRegisters #-}
useRegisters :: Getting b Registers b -> MonadAtari b
useRegisters lens = do
    atari <- ask
    registers' <- liftIO $ readIORef (atari ^. regs)
    return $! registers' ^. lens

{-# INLINE putRegisters #-}
putRegisters :: ASetter Registers Registers a a -> a -> MonadAtari ()
putRegisters lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. regs) (set lens value)

{-# INLINE modifyRegisters #-}
modifyRegisters lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. regs) (over lens modifier)

{-# INLINE zoomRegisters #-}
zoomRegisters :: StateT Registers IO a -> MonadAtari a
zoomRegisters m = do
    atari <- ask
    registers' <- liftIO $ readIORef (atari ^. regs)
    (a, registers'') <- liftIO $ runStateT m registers'
    liftIO $ writeIORef (atari ^. regs) registers''
    return a

{-# INLINE useClock #-}
useClock :: Getting b Int64 b -> MonadAtari b
useClock lens = do
    atari <- ask
    clock' <- liftIO $ readIORef (atari ^. clock)
    return $! clock' ^. lens

{-# INLINE putClock #-}
putClock :: ASetter Int64 Int64 a a -> a -> MonadAtari ()
putClock lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (set lens value)

{-# INLINE modifyClock #-}
modifyClock lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (over lens modifier)

{-# INLINE useSprites #-}
useSprites :: Getting b Sprites b -> MonadAtari b
useSprites lens = do
    atari <- ask
    sprites' <- liftIO $ readIORef (atari ^. sprites)
    return $! sprites' ^. lens

{-# INLINE putSprites #-}
putSprites :: ASetter Sprites Sprites a a -> a -> MonadAtari ()
putSprites lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. sprites) (set lens value)

{-# INLINE modifySprites #-}
modifySprites lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. sprites) (over lens modifier)

{-# INLINE useIntervalTimer #-}
useIntervalTimer :: Getting b IntervalTimer b -> MonadAtari b
useIntervalTimer lens = do
    atari <- ask
    intervalTimer' <- liftIO $ readIORef (atari ^. intervalTimer)
    return $! intervalTimer' ^. lens

{-# INLINE putIntervalTimer #-}
putIntervalTimer :: ASetter IntervalTimer IntervalTimer a a -> a -> MonadAtari ()
putIntervalTimer lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. intervalTimer) (set lens value)

{-# INLINE modifyIntervalTimer #-}
modifyIntervalTimer lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. intervalTimer) (over lens modifier)

{-# INLINE useGraphics #-}
useGraphics :: Getting b Graphics b -> MonadAtari b
useGraphics lens = do
    atari <- ask
    graphics' <- liftIO $ readIORef (atari ^. graphics)
    return $! graphics' ^. lens

{-# INLINE putGraphics #-}
putGraphics :: ASetter Graphics Graphics a a -> a -> MonadAtari ()
putGraphics lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. graphics) (set lens value)

{-# INLINE modifyGraphics #-}
modifyGraphics lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. graphics) (over lens modifier)

{-# INLINE useStellaClock #-}
useStellaClock :: Getting b Int64 b -> MonadAtari b
useStellaClock lens = do
    atari <- ask
    stellaClock' <- liftIO $ readIORef (atari ^. stellaClock)
    return $! stellaClock' ^. lens

{-# INLINE putStellaClock #-}
putStellaClock :: ASetter Int64 Int64 a a -> a -> MonadAtari ()
putStellaClock lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaClock) (set lens value)

{-# INLINE modifyStellaClock #-}
modifyStellaClock lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaClock) (over lens modifier)

{-# INLINE getORegisters #-}
getORegisters :: MonadAtari ORegArray
getORegisters = do
    atari <- ask
    return $ atari ^. oregisters

{-# INLINE getIRegisters #-}
getIRegisters :: MonadAtari IRegArray
getIRegisters = do
    atari <- ask
    return $ atari ^. iregisters
