{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Atari2600(MonadAtari(..),
                 {-hardware,-}
                 rom,
                 ram,
                 graphics,
                 useStellaDebug,
                 putStellaDebug,
                 modifyStellaDebug,
                 useMemory,
                 word8Array,
                 intArray,
                 boolArray,
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
                 -- useIntervalTimer,
                 -- putIntervalTimer,
                 -- modifyIntervalTimer,
                 useGraphics,
                 putGraphics,
                 modifyGraphics,
                 getORegisters,
                 getIRegisters,
                 getBoolArray,
                 getIntArray,
                 getInt64Array,
                 getWord64Array,
                 stellaDebug,
                 getBackSurface,
                 getFrontSurface,
                 getFrontWindow,
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
                 -- position,
                 -- stellaSDL,
                 {- pf, -}
                 oregisters,
                 iregisters,
                 {- trigger1, -}
                 --Hardware(..),
                 Atari2600(..),
                 Registers(..)) where

import Data.Word
import Data.Int
import Stella.Graphics
import Stella.Sprites
import SDL.Vect
import SDL.Video
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

data Atari2600 = Atari2600 {
    _memory :: IORef Memory,
    _ram :: IOUArray Int Word8,
    _rom :: IOUArray Int Word8,
    _stellaDebug :: IORef DebugState,
    _regs :: IORef Registers,
    _clock :: IORef Int64,
    _debug :: IORef Int,
    _sprites :: IORef Sprites,
    -- _intervalTimer :: IORef IntervalTimer,
    _graphics :: IORef Graphics,
    _stellaClock :: IORef Int64,
    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8,
    _sdlBackSurface :: Surface,
    _sdlFrontSurface :: Surface,
    _sdlFrontWindow :: Window,
    _boolArray :: Segment Bool,
    _intArray :: Segment Int,
    _int64Array :: Segment Int64,
    _word64Array :: Segment Word64,
    _word8Array :: Segment Word8
}

$(makeLenses ''Atari2600)
$(makeLenses ''Registers)

newtype MonadAtari a = M { unM :: ReaderT Atari2600 IO a }
      deriving (Functor, Applicative, Monad, MonadReader Atari2600, MonadIO)

{-# INLINE useStellaDebug #-}
useStellaDebug :: Getting b DebugState b -> MonadAtari b
useStellaDebug lens = do
    atari <- ask
    stellaDebug' <- liftIO $ readIORef (atari ^. stellaDebug)
    return $! stellaDebug' ^. lens

{-# INLINE putStellaDebug #-}
putStellaDebug :: ASetter DebugState DebugState a a -> a -> MonadAtari ()
putStellaDebug lens value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (set lens value)

{-# INLINE modifyStellaDebug #-}
modifyStellaDebug lens modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (over lens modifier)

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

{-
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
-}

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

{-# INLINE getBoolArray #-}
getBoolArray :: MonadAtari (Segment Bool)
getBoolArray = do
    atari <- ask
    return $ atari ^. boolArray

{-# INLINE getIntArray #-}
getIntArray :: MonadAtari (Segment Int)
getIntArray = do
    atari <- ask
    return $ atari ^. intArray

{-# INLINE getInt64Array #-}
getInt64Array :: MonadAtari (Segment Int64)
getInt64Array = do
    atari <- ask
    return $ atari ^. int64Array

{-# INLINE getWord64Array #-}
getWord64Array :: MonadAtari (Segment Word64)
getWord64Array = do
    atari <- ask
    return $ atari ^. word64Array

{-# INLINE getIRegisters #-}
getIRegisters :: MonadAtari IRegArray
getIRegisters = do
    atari <- ask
    return $ atari ^. iregisters

{-# INLINE getBackSurface #-}
getBackSurface :: MonadAtari Surface
getBackSurface = do
    atari <- ask
    return $ atari ^. sdlBackSurface

{-# INLINE getFrontSurface #-}
getFrontSurface :: MonadAtari Surface
getFrontSurface = do
    atari <- ask
    return $ atari ^. sdlFrontSurface

{-# INLINE getFrontWindow #-}
getFrontWindow :: MonadAtari Window
getFrontWindow = do
    atari <- ask
    return $ atari ^. sdlFrontWindow
