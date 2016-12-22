{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Atari2600(
                 MonadAtari(..),
                 Atari2600(..),
                 load,
                 store,
                 modify,
                 useStellaDebug,
                 modifyClock,
                 useStellaClock,
                 getBackSurface,
                 ram,
                 rom,
                 useMemory,
                 useClock,
                 putStellaDebug,
                 getFrontSurface,
                 getFrontWindow,
                 modifyStellaClock,
                 stellaDebug,
                 clock,
                 stellaClock,
                 modifyStellaDebug
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
import Asm

data Atari2600 = Atari2600 {
    _memory :: IORef Memory,
    _clock :: IORef Int64,
    _stellaClock :: IORef Int64,
    _stellaDebug :: IORef DebugState,

    _ram :: IOUArray Int Word8,
    _rom :: IOUArray Int Word8,
    _boolArray :: Segment Bool,
    _intArray :: Segment Int,
    _word8Array :: Segment Word8,
    _word16Array :: Segment Word16,
    _word64Array :: Segment Word64,

    _sdlBackSurface :: Surface,
    _sdlFrontSurface :: Surface,
    _sdlFrontWindow :: Window
}

$(makeLenses ''Atari2600)

newtype MonadAtari a = M { unM :: ReaderT Atari2600 IO a }
      deriving (Functor, Applicative, Monad, MonadReader Atari2600, MonadIO)

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

instance Reg Word64 MonadAtari where
    {-# INLINE load #-}
    load r = do
        value <- view word64Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word64Array
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

{-# INLINE useClock #-}
useClock :: Getting b Int64 b -> MonadAtari b
useClock lens' = do
    atari <- ask
    clock' <- liftIO $ readIORef (atari ^. clock)
    return $! clock' ^. lens'

{-# INLINE modifyClock #-}
modifyClock :: ASetter Int64 Int64 a b -> (a -> b) -> MonadAtari ()
modifyClock lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (over lens' modifier)

{-# INLINE useStellaClock #-}
useStellaClock :: Getting b Int64 b -> MonadAtari b
useStellaClock lens' = do
    atari <- ask
    stellaClock' <- liftIO $ readIORef (atari ^. stellaClock)
    return $! stellaClock' ^. lens'

{-# INLINE modifyStellaClock #-}
modifyStellaClock :: ASetter Int64 Int64 a b -> (a -> b) -> MonadAtari ()
modifyStellaClock lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaClock) (over lens' modifier)

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
