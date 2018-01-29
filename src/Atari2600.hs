{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Atari2600(
                 MonadAtari(..),
                 Atari2600(..),
                 xscale,
                 yscale,
                 load,
                 store,
                 modify,
                 useStellaDebug,
                 modifyClock,
                 useStellaClock,
                 bankState,
                 ram,
#if TRACE
                 record,
                 recordPtr,
#endif
                 rom,
                 useClock,
                 putStellaDebug,
                 sdlWindow,
                 textureData,
                 windowWidth,
                 windowHeight,
                 tex,
                 glProg,
                 glAttrib,
                 modifyStellaClock,
                 stellaDebug,
                 clock,
                 stellaClock,
                 delays,
                 modifyStellaDebug
                 ) where

import Control.Lens
import Control.Monad.Reader
import Data.Array.Base
#if TRACE
import Data.Array.Storable
#endif
import Data.Array.IO
import Data.IORef
import Data.Int
import Data.Word
import qualified SDL
import Foreign.Ptr
import DebugState
import Memory
import qualified Graphics.Rendering.OpenGL as GL
import Asm

data Atari2600 = Atari2600 {
    _bankState :: IORef BankState,
    _clock :: IORef Int64,
    _stellaClock :: IORef Int64,
    _stellaDebug :: IORef DebugState,

    _ram :: IOUArray Int Word8,
#if TRACE
    _record :: StorableArray Int Word8,
    _recordPtr :: IORef Int,
#endif
    _rom :: IOUArray Int Word8,
    _boolArray :: Segment Bool,
    _intArray :: Segment Int,
    _word8Array :: Segment Word8,
    _word16Array :: Segment Word16,
    _word64Array :: Segment Word64,

    _sdlWindow :: !SDL.Window,
    _textureData :: Ptr Word8,
    _tex :: !GL.TextureObject,
    _glProg :: !GL.Program,
    _glAttrib :: !GL.AttribLocation,
    _windowWidth :: !Int,
    _windowHeight :: !Int,
    _xscale :: !Int,
    _yscale :: !Int,

    _delays :: IOUArray Word16 Int
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

{-
{-# INLINE useMemory #-}
useMemory :: Getting b Memory b -> MonadAtari b
useMemory lens' = do
    atari <- ask
    memory' <- liftIO $ readIORef (atari ^. memory)
    return $! memory' ^. lens'
-}

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

{-
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
    -}
