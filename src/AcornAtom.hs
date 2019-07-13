{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}

module AcornAtom(
                 MonadAcorn(..),
                 AcornAtom(..),
                 Controllers(..),
                 getX,
                 putX,
                 getPC,
                 putC,
                 getC,
                 putZ,
                 getZ,
                 putI,
                 getI,
                 putD,
                 getD,
                 putB,
                 getB,
                 putV,
                 getV,
                 putN,
                 getN,
                 getA,
                 putA,
                 getS,
                 putS,
                 getP,
                 putP,
                 getY,
                 putY,
                 putPC,
                 addPC,
                 xscale,
                 yscale,
                 load,
                 store,
                 modify,
                 useStellaDebug,
                 modifyClock,
                 controllers,
                 ram,
                 rom,
                 nextFrameTime,
                 useClock,
                 putStellaDebug,
                 sdlWindow,
                 textureData,
                 lastTextureData,
                 windowWidth,
                 windowHeight,
                 tex,
                 lastTex,
                 glProg,
                 glAttrib,
                 stellaDebug,
                 clock,
                 delays,
                 modifyStellaDebug
                 ) where

import Control.Lens
import Control.Monad.Reader
import System.Clock
import Data.Array.Base
import Data.Array.IO
import Data.Bits.Lens
import Data.IORef
import Data.Int
import Data.Word
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Ptr
import DebugState
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL
import Asm
import Codec.Serialise

-- Need to make left and right separately configurable
data Controllers = Joysticks | Keypads deriving (Eq, Show, Read)

data AcornAtom = AcornAtom {
    _clock :: IORef Int64,
    _stellaDebug :: IORef DebugState,

    _nextFrameTime :: IORef TimeSpec,

    _ram :: IOUArray Int Word8,
    _rom :: IOUArray Int Word8,
    _boolArray :: Segment Bool,
    _intArray :: Segment Int,
    _word8Array :: Segment Word8,
    _word16Array :: Segment Word16,
    _word64Array :: Segment Word64,

    _controllers :: Controllers,

    _sdlWindow :: !GLFW.Window,
    _textureData :: Ptr Word8,
    _lastTextureData :: Ptr Word8,
    _tex :: !GL.TextureObject,
    _lastTex :: !GL.TextureObject,
    _glProg :: !GL.Program,
    _glAttrib :: !GL.AttribLocation,
    _windowWidth :: !Int,
    _windowHeight :: !Int,
    _xscale :: !Int,
    _yscale :: !Int,

    _delays :: IOUArray Word16 Int
} deriving Generic

instance Serialise AcornAtom

$(makeLenses ''AcornAtom)

newtype MonadAcorn a = M { unM :: ReaderT AcornAtom IO a }
      deriving (Functor, Applicative, Monad, MonadReader AcornAtom, MonadIO)

-- Do I still want Reg type class?
instance Reg Word8 MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view word8Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word8Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Int MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view intArray
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view intArray
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Word16 MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view word16Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word16Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Word64 MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view word64Array
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view word64Array
        liftIO $ unsafeWrite value (unTyped r) v

instance Reg Bool MonadAcorn where
    {-# INLINE load #-}
    load r = do
        value <- view boolArray
        liftIO $ unsafeRead value (unTyped r)
    {-# INLINE store #-}
    store r v = do
        value <- view boolArray
        liftIO $ unsafeWrite value (unTyped r) v

{-# INLINE useStellaDebug #-}
useStellaDebug :: Getting b DebugState b -> MonadAcorn b
useStellaDebug lens' = do
    atari <- ask
    stellaDebug' <- liftIO $ readIORef (atari ^. stellaDebug)
    return $! stellaDebug' ^. lens'

{-# INLINE putStellaDebug #-}
putStellaDebug :: ASetter DebugState DebugState a a -> a -> MonadAcorn ()
putStellaDebug lens' value = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (set lens' value)

{-# INLINE modifyStellaDebug #-}
modifyStellaDebug :: ASetter DebugState DebugState a b -> (a -> b) -> MonadAcorn ()
modifyStellaDebug lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. stellaDebug) (over lens' modifier)

{-# INLINE useClock #-}
useClock :: Getting b Int64 b -> MonadAcorn b
useClock lens' = do
    atari <- ask
    clock' <- liftIO $ readIORef (atari ^. clock)
    return $! clock' ^. lens'

{-# INLINE modifyClock #-}
modifyClock :: ASetter Int64 Int64 a b -> (a -> b) -> MonadAcorn ()
modifyClock lens' modifier = do
    atari <- ask
    liftIO $ modifyIORef' (atari ^. clock) (over lens' modifier)

-- 6502 registers

-- {-# INLINE getX #-}
getX :: MonadAcorn Word8
getX = load x

-- {-# INLINE putX #-}
putX :: Word8 -> MonadAcorn ()
putX r = x @= r 

getPC :: MonadAcorn Word16
putC :: Bool -> MonadAcorn ()
getC :: MonadAcorn Bool
putZ :: Bool -> MonadAcorn ()
getZ :: MonadAcorn Bool
putI :: Bool -> MonadAcorn ()
getI :: MonadAcorn Bool
putD :: Bool -> MonadAcorn ()
getD :: MonadAcorn Bool
putB :: Bool -> MonadAcorn ()
getB :: MonadAcorn Bool
putV :: Bool -> MonadAcorn ()
getV :: MonadAcorn Bool
putN :: Bool -> MonadAcorn ()
getN :: MonadAcorn Bool
getA :: MonadAcorn Word8
putA :: Word8 -> MonadAcorn ()
getS :: MonadAcorn Word8
putS :: Word8 -> MonadAcorn ()
getP :: MonadAcorn Word8
putP :: Word8 -> MonadAcorn ()
getY :: MonadAcorn Word8
putY :: Word8 -> MonadAcorn ()
putPC :: Word16 -> MonadAcorn ()
addPC :: Int -> MonadAcorn ()
--
-- {-# INLINE getPC #-}
getPC = load pc
-- {-# INLINE putC #-}
putC b = do { p' <- load p; p @= (p' & bitAt 0 .~ b) }
-- {-# INLINE getC #-}
getC = do { p' <- load p; return (p' ^. bitAt 0) }
-- {-# INLINE putZ #-}
putZ b = do { p' <- load p; p @= (p' & bitAt 1 .~ b) }
-- {-# INLINE getZ #-}
getZ = do { p' <- load p; return (p' ^. bitAt 1) }
-- {-# INLINE putI #-}
putI b = do { p' <- load p; p @= (p' & bitAt 2 .~ b) }
-- {-# INLINE getI #-}
getI = do { p' <- load p; return (p' ^. bitAt 2) }
-- {-# INLINE putD #-}
putD b = do { p' <- load p; p @= (p' & bitAt 3 .~ b) }
-- {-# INLINE getD #-}
getD = do { p' <- load p; return (p' ^. bitAt 3) }
-- {-# INLINE putB #-}
putB b = do { p' <- load p; p @= (p' & bitAt 4 .~ b) }
-- {-# INLINE getB #-}
getB = do { p' <- load p; return (p' ^. bitAt 4) }
-- {-# INLINE putV #-}
putV b = do { p' <- load p; p @= (p' & bitAt 6 .~ b) }
-- {-# INLINE getV #-}
getV = do { p' <- load p; return (p' ^. bitAt 6) }
-- {-# INLINE putN #-}
putN b = do { p' <- load p; p @= (p' & bitAt 7 .~ b) }
-- {-# INLINE getN #-}
getN = do { p' <- load p; return (p' ^. bitAt 7) }
-- {-# INLINE getA #-}
getA = load a
-- {-# INLINE putA #-}
putA r = a @= r
-- {-# INLINE getS #-}
getS = load s
-- {-# INLINE putS #-}
putS r = s @= r
-- {-# INLINE getP #-}
getP = load p
-- {-# INLINE putP #-}
putP r = p @= r 
-- {-# INLINE getY #-}
getY = load y
-- {-# INLINE putY #-}
putY r = y @= r
-- {-# INLINE putPC #-}
putPC r = pc @= r
-- {-# INLINE addPC #-}
addPC n = modify pc (+ fromIntegral n)
