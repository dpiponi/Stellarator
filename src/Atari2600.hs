{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}

module Atari2600(MonadAtari(..),
                 MyState(..),
                 hardware,
                 graphics,
                 useHardware,
                 putHardware,
                 zoomHardware,
                 useMemory,
                 putMemory,
                 zoomMemory,
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
    _stellaClock :: !Int64,
    _graphics :: Graphics,
    _sprites :: Sprites,
    _intervalTimer :: IntervalTimer,
    _trigger1 :: !Bool,
    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8,
    _stellaSDL :: SDLState,
    _pf :: !Word64
}

$(makeLenses ''Hardware)

data Atari2600 = Atari2600 {
    _memory :: IORef Memory,
    _hardware :: IORef Hardware,
    _regs :: IORef Registers,
    _clock :: IORef Int64,
    _debug :: IORef Int
}

$(makeLenses ''Atari2600)
$(makeLenses ''Registers)

newtype MonadAtari a = M { unM :: ReaderT Atari2600 IO a }
      deriving (Functor, Applicative, Monad, MonadReader Atari2600, MonadIO)

--newtype MonadAtari a = M { unM :: StateT Atari2600 IO a }
--    deriving (Functor, Applicative, Monad, MonadState Atari2600, MonadIO)

data SP a b = SP !a !b
newtype MyState s a = S { runS :: s -> IO (SP s a) }

instance Functor (MyState s) where
    {-# INLINE fmap #-}
    fmap f (S g) = S $ \s -> do { SP s' a <- g s ; return (SP s' (f a)) }

instance Applicative (MyState s) where
    {-# INLINE pure #-}
    pure a = S $ \s -> return $! SP s a
    {-# INLINE (<*>) #-}
    S f <*> S x = S $ \s -> do { SP s' a' <- f s; SP s'' a'' <- x s'; return (SP s'' (a' a'')) }

instance Monad (MyState s) where
    {-# INLINE return #-}
    return a = S $ \s -> return $! SP s a
    {-# INLINE (>>=) #-}
    S f >>= g = S $ \ !s -> do { SP s' a' <- f s; runS (g a') $! s' }

instance MonadState s (MyState s) where
    {-# INLINE get #-} 
    get = S $ \ !s -> return $! SP s s
    {-# INLINE put #-} 
    put a = S $ \ !s -> return $! SP a ()

instance MonadIO (MyState s) where
    {-# INLINE liftIO #-}
    liftIO m = S $ \ !s -> do { m' <- m; return $! SP s m' }

{-
newtype MonadAtari a = M { unM :: MyState Atari2600 a }
    deriving (Functor, Applicative, Monad, MonadState Atari2600, MonadIO)

{-# INLINE zoomMemory #-}
zoomMemory :: MyState Memory a -> MonadAtari a
zoomMemory (S m) = do
    s <- get
    SP mem' a <- liftIO $ m (_memory s)
    put $ s { _memory = mem' }
    return a
-}

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

{-# INLINE zoomHardware #-}
zoomHardware :: MyState Hardware a -> MonadAtari a
zoomHardware (S m) = do
    atari <- ask
    hardware' <- liftIO $ readIORef (atari ^. hardware)
    SP hardware'' a <- liftIO $ m hardware'
    liftIO $ writeIORef (atari ^. hardware) hardware''
    return a

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

{-# INLINE zoomMemory #-}
zoomMemory :: MyState Memory a -> MonadAtari a
zoomMemory (S m) = do
    atari <- ask
    memory' <- liftIO $ readIORef (atari ^. memory)
    SP memory'' a <- liftIO $ m memory'
    liftIO $ writeIORef (atari ^. memory) memory''
    return a
