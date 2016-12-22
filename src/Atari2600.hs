{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Atari2600(
                 Atari2600(..),
                 MonadAtari(..),
                 boolArray,
                 clock,
                 debug,
                 getBackSurface,
                 getBoolArray,
                 getFrontSurface,
                 getFrontWindow,
                 getIRegisters,
                 getORegisters,
                 getWord64Array,
                 --graphics,
                 intArray,
                 iregisters,
                 load,
                 memory,
                 modify,
                 modifyClock,
                 modifyMemory,
                 --modifySprites,
                 modifyStellaClock,
                 modifyStellaDebug,
                 oregisters,
                 putClock,
                 --putSprites,
                 putStellaClock,
                 putStellaDebug,
                 ram,
                 rom,
                 --sprites,
                 stellaClock,
                 stellaDebug,
                 store,
                 useClock,
                 useMemory,
                 --useSprites,
                 useStellaClock,
                 useStellaDebug,
                 word16Array,
                 word8Array
                 ) where

import Control.Lens
import Control.Monad.Reader
import Data.Array.Base
import Data.Array.IO
import Data.IORef
import Data.Int
import Data.Word
import DebugState
import Memory
import SDL.Video
import Stella.TIARegisters

data Atari2600 = Atari2600 {
    _memory :: IORef Memory,
    _clock :: IORef Int64,
    -- _debug :: IORef Int,
    _stellaClock :: IORef Int64,
    _stellaDebug :: IORef DebugState,

    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8,
    _ram :: IOUArray Int Word8,
    _rom :: IOUArray Int Word8,
    _boolArray :: Segment Bool,
    _intArray :: Segment Int,
    _word64Array :: Segment Word64,
    _word16Array :: Segment Word16,
    _word8Array :: Segment Word8,

    _sdlBackSurface :: Surface,
    _sdlFrontSurface :: Surface,
    _sdlFrontWindow :: Window
}

$(makeLenses ''Atari2600)

newtype MonadAtari a = M { unM :: ReaderT Atari2600 IO a }
      deriving (Functor, Applicative, Monad, MonadReader Atari2600, MonadIO)

class Monad m => Reg t m where
    load :: TypedIndex t -> m t
    store :: TypedIndex t -> t -> m ()
    modify :: TypedIndex t -> (t -> t) -> m ()
    modify r f = do { value <- load r; store r (f value) }

instance Reg Word8 MonadAtari where
    {-# INLINE load #-}
    load r = do
        value <- view word8Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word8Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Int MonadAtari where
    {-# INLINE load #-}
    load r = do
        value <- view intArray
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view intArray
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Word16 MonadAtari where
    {-# INLINE load #-}
    load r = do
        value <- view word16Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word16Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Bool MonadAtari where
    {-# INLINE load #-}
    load r = do
        value <- view boolArray
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view boolArray
        liftIO $ unsafeWrite value (unTyped r) v

{-# INLINE useStellaDebug #-}
useStellaDebug :: Getting b DebugState b -> MonadAtari b
useStellaDebug lens' = do
    atari <- ask
    stellaDebug' <- liftIO $ readIORef (atari ^. stellaDebug)
    return $! stellaDebug' ^. lens'

{-# INLINE putStellaDebug #-}
putStellaDebug :: ASetter DebugState DebugState a a -> a -> MonadAtari ()
putStellaDebug lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (set lens' value)

{-# INLINE modifyStellaDebug #-}
modifyStellaDebug :: ASetter DebugState DebugState a b -> (a -> b) -> MonadAtari ()
modifyStellaDebug lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (over lens' modifier)

{-# INLINE useMemory #-}
useMemory :: Getting b Memory b -> MonadAtari b
useMemory lens' = do
    atari <- ask
    memory' <- liftIO $ readIORef (atari ^. memory)
    return $! memory' ^. lens'

{-# INLINE modifyMemory #-}
modifyMemory lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. memory) (over lens' modifier)

{-# INLINE useClock #-}
useClock :: Getting b Int64 b -> MonadAtari b
useClock lens' = do
    atari <- ask
    clock' <- liftIO $ readIORef (atari ^. clock)
    return $! clock' ^. lens'

{-# INLINE putClock #-}
putClock :: ASetter Int64 Int64 a a -> a -> MonadAtari ()
putClock lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (set lens' value)

{-# INLINE modifyClock #-}
modifyClock lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (over lens' modifier)

{-
{-# INLINE useSprites #-}
useSprites :: Getting b Sprites b -> MonadAtari b
useSprites lens' = do
    atari <- ask
    sprites' <- liftIO $ readIORef (atari ^. sprites)
    return $! sprites' ^. lens'

{-# INLINE putSprites #-}
putSprites :: ASetter Sprites Sprites a a -> a -> MonadAtari ()
putSprites lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. sprites) (set lens' value)

{-# INLINE modifySprites #-}
modifySprites lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. sprites) (over lens' modifier)
    -}

{-
{-# INLINE useGraphics #-}
useGraphics :: Getting b Graphics b -> MonadAtari b
useGraphics lens' = do
    atari <- ask
    graphics' <- liftIO $ readIORef (atari ^. graphics)
    return $! graphics' ^. lens'

{-# INLINE putGraphics #-}
putGraphics :: ASetter Graphics Graphics a a -> a -> MonadAtari ()
putGraphics lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. graphics) (set lens' value)
    -}

{-# INLINE useStellaClock #-}
useStellaClock :: Getting b Int64 b -> MonadAtari b
useStellaClock lens' = do
    atari <- ask
    stellaClock' <- liftIO $ readIORef (atari ^. stellaClock)
    return $! stellaClock' ^. lens'

{-# INLINE putStellaClock #-}
putStellaClock :: ASetter Int64 Int64 a a -> a -> MonadAtari ()
putStellaClock lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaClock) (set lens' value)

{-# INLINE modifyStellaClock #-}
modifyStellaClock lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaClock) (over lens' modifier)

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
