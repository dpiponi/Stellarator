{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Binary
import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Lens hiding (_last)
import Control.Monad
import Text.Parsec
import Control.Monad.State
import Data.Array.IO
import Data.Array.Unboxed
import Data.Binary hiding (get)
import System.Exit
import Data.Binary.Get
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.ByteString as B hiding (last, putStr, putStrLn, getLine, length, elem, map, reverse)
import Data.Char
import Data.Int
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import System.Random
import Foreign.Storable
import Numeric
import SDL.Event
import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import System.Console.CmdArgs hiding ((+=))
import System.IO
--import System.Random
import TIAColors
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified SDL
import Debug.Trace
import Prelude hiding (last)
import Core
import Disasm
import System.Console.Haskeline
import Control.Concurrent

import DebugCmd
import MemoryMap
import Stella.Graphics
import Stella.Sprites

newtype OReg = OReg Word16 deriving (Ord, Ix, Eq, Num)
newtype IReg = IReg Word16 deriving (Ord, Ix, Eq, Num)

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

data IntervalTimer = IntervalTimer {
    _intim :: !Word8,
    _subtimer :: !CInt,
    _interval :: !CInt
}

$(makeLenses ''IntervalTimer)

data StellaClock = Clock {
    _now :: !Int64,
    _last :: !Int64
}

$(makeLenses ''StellaClock)

data StellaDebug = Debug {
    _debugLevel :: !Int,
    _posbreak :: (CInt, CInt),
    _variables :: Map.Map String Value
    -- _xbreak :: !Int32,
    -- _ybreak :: !Int32
}

$(makeLenses '' StellaDebug)

data StellaSDL = StellaSDL {
    _sdlBackSurface :: !Surface,
    _sdlFrontSurface :: !Surface,
    _sdlFrontWindow :: !SDL.Window
}

$(makeLenses '' StellaSDL)

data StateAtari = S {
     _mem :: IOUArray Int Word8,
     _regs :: !Registers,
     _clock :: !Int,
     _debug :: !Int,
     _oregisters :: IOUArray OReg Word8,
     _iregisters :: IOUArray IReg Word8,

    _stellaDebug :: StellaDebug,
    _stellaSDL :: StellaSDL,

    _position :: (CInt, CInt),
    _stellaClock :: StellaClock,
    _graphics :: Graphics,
    _sprites :: Sprites,
    _intervalTimer :: IntervalTimer
}

$(makeLenses ''StateAtari)
$(makeLenses ''Registers)

{-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

{-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

{-# INLINE iz #-}
iz :: Word16 -> Int -- or NUM
iz = fromIntegral

nusiz0, nusiz1, colup0, colup1, pf0, pf1, pf2, enam0, enam1, hmp0, hmp1, hmm0, hmm1, hmbl :: OReg
vblank, vsync, refp0, refp1, colupf, colubk, ctrlpf, resmp0, resmp1 :: OReg
vsync = 0x00
vblank = 0x01
nusiz0 = 0x04
nusiz1 = 0x05
colup0 = 0x06
colup1 = 0x07
colupf = 0x08
colubk = 0x09
ctrlpf = 0x0a
refp0 = 0x0b
refp1 = 0x0c
pf0 = 0x0d
pf1 = 0x0e
pf2 = 0x0f
enam0 = 0x1d
enam1 = 0x1e
hmp0 = 0x20
hmp1 = 0x21
hmm0 = 0x22
hmm1 = 0x23
hmbl = 0x24
resmp0 = 0x28
resmp1 = 0x29

cxm0p, cxm1p, cxp0fb, cxp1fb, cxm0fb, cxm1fb, cxblpf, cxppmm, inpt4, inpt5 :: IReg
cxm0p = 0x00
cxm1p = 0x01
cxp0fb = 0x02
cxp1fb = 0x03
cxm0fb = 0x04
cxm1fb = 0x05
cxblpf = 0x06
cxppmm = 0x07
inpt4 = 0x0c
inpt5 = 0x0d

swcha, swchb :: IReg
swcha = 0x280
swchb = 0x282

{-# INLINE backSurface #-}
{-# INLINE frontSurface #-}
{-# INLINE frontWindow #-}
backSurface :: Lens' StateAtari Surface
frontSurface :: Lens' StateAtari Surface
frontWindow :: Lens' StateAtari SDL.Window
backSurface = stellaSDL . sdlBackSurface
frontSurface = stellaSDL . sdlFrontSurface
frontWindow = stellaSDL . sdlFrontWindow

{-# INLINE hpos #-}
{-# INLINE vpos #-}
hpos, vpos :: Lens' StateAtari CInt
hpos = position . _1
vpos = position . _2

{-# INLINE ppos0 #-}
{-# INLINE ppos1 #-}
{-# INLINE mpos0 #-}
{-# INLINE mpos1 #-}
{-# INLINE bpos #-}
ppos0, ppos1, mpos0, mpos1, bpos :: Lens' StateAtari CInt
ppos0 = sprites . s_ppos0
ppos1 = sprites . s_ppos1
mpos0 = sprites . s_mpos0
mpos1 = sprites . s_mpos1
bpos = sprites . s_bpos

{-# INLINE nowClock #-}
{-# INLINE lastClock #-}
nowClock, lastClock :: Lens' StateAtari Int64
nowClock = stellaClock . now
lastClock = stellaClock . last

{- INLINE stellaDebugStr -}
stellaDebugStr :: (MonadIO m, MonadState StateAtari m) =>
                  Int -> String -> m ()
stellaDebugStr n str = do
    d <- use (stellaDebug . debugLevel)
    if n <= d
        then do
            before <- use lastClock
            now <- use nowClock
            liftIO $ putStr $ show now ++ " +" ++ show (now-before) ++ ": " ++ str
            lastClock .= now
        else return ()

{- INLINE stellaDebugStrLn -}
stellaDebugStrLn :: (MonadIO m, MonadState StateAtari m) =>
                    Int -> String -> m ()
stellaDebugStrLn n str = do
    d <- use (stellaDebug . debugLevel)
    if n <= d
        then do
            before <- use lastClock
            now <- use nowClock
            liftIO $ putStrLn $ show now ++ " +" ++ show (now-before) ++ ": " ++ str
            lastClock .= now
        else return ()

{-# INLINE putORegister #-}
putORegister :: (MonadIO m, MonadState StateAtari m) => OReg -> Word8 -> m ()
putORegister i v = do
    r <- use oregisters
    liftIO $ writeArray r i v

{-# INLINE getORegister #-}
getORegister :: (MonadIO m, MonadState StateAtari m) => OReg -> m Word8
getORegister i = do
    r <- use oregisters
    liftIO $ readArray r i

{-# INLINE fastGetORegister #-}
fastGetORegister :: IOUArray OReg Word8 -> OReg -> IO Word8
fastGetORegister = readArray

{-# INLINE putIRegister #-}
putIRegister :: (MonadIO m, MonadState StateAtari m) => IReg -> Word8 -> m ()
putIRegister i v = do
    r <- use iregisters
    liftIO $ writeArray r i v

{-# INLINE modifyIRegister #-}
modifyIRegister :: (MonadIO m, MonadState StateAtari m) => IReg -> (Word8 -> Word8) -> m ()
modifyIRegister i f = do
    r <- use iregisters
    liftIO $ (readArray r i >>= writeArray r i . f)

{-# INLINE fastModifyIRegister #-}
fastModifyIRegister :: IOUArray IReg Word8 -> IReg -> (Word8 -> Word8) -> IO ()
fastModifyIRegister r i f = readArray r i >>= writeArray r i . f

{-# INLINE getIRegister #-}
getIRegister :: (MonadIO m, MonadState StateAtari m) => IReg -> m Word8
getIRegister i = do
    r <- use iregisters
    liftIO $ readArray r i

{-# INLINE orIRegister #-}
orIRegister :: (MonadIO m, MonadState StateAtari m) => IReg -> Word8 -> m ()
orIRegister i v = modifyIRegister i (v .|.)

{-# INLINE fastOrIRegister #-}
fastOrIRegister :: IOUArray IReg Word8 -> IReg -> Word8 -> IO ()
fastOrIRegister r i v = fastModifyIRegister r i (v .|.)

inBinary :: (Bits a) => Int -> a -> String
inBinary 0 x = ""
inBinary n x = inBinary (n-1) (x `shift` (-1)) ++ if testBit x 0 then "1" else "0"

explainNusiz :: Word8 -> String
explainNusiz nusiz =
    case nusiz .&. 0b111 of
        0b000 -> "one copy"
        0b001 -> "two copies - close"
        0b010 -> "two copies - med"
        0b011 -> "three copies - close"
        0b100 -> "two copies - wide"
        0b101 -> "double size player"
        0b110 -> "3 copies medium"
        0b111 -> "quad sized player"

{- INLINE playfield -}
playfield :: IOUArray OReg Word8 -> Word8 -> Int -> IO Bool
playfield r ctrlpf' i | i >= 0 && i < 4 = flip testBit (i+4) <$> fastGetORegister r pf0
                      | i >=4 && i < 12 = flip testBit (11-i) <$> fastGetORegister r pf1
                      | i >= 12 && i < 20 = flip testBit (i-12) <$> fastGetORegister r pf2
playfield r ctrlpf' i | i >= 20 && i < 40 = playfield r ctrlpf' $ if testBit ctrlpf' 0 then 39-i else i-20

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True x = x
flipIf False x = 7-x

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Word8 -> CInt -> Word8 -> Bool
stretchPlayer reflect sizeCopies o bitmap =
    if o < 0 || o >= 72
        then False
        else case sizeCopies of
            0b000 -> -- one copy
                if o < 8
                    then testBit bitmap (flipIf reflect $ fromIntegral o)
                    else False
            0b001 -> -- two copies close
                if o < 8 || o >= 16 && o < 24
                    then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                    else False
            0b010 -> -- two copies - med
                if o < 8 || o >= 32 && o < 40
                    then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                    else False
            0b011 -> -- three copies close
                if o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40
                    then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                    else False
            0b100 -> -- two copies wide
                if o < 8 || o >= 64
                    then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                    else False
            0b101 -> -- double size player
                if o < 16
                    then testBit bitmap (flipIf reflect $ fromIntegral ((o `shift` (-1)) .&. 7))
                    else False
            0b110 -> -- three copies medium
                if o < 8 || o >= 32 && o < 40 || o >= 64
                    then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                    else False
            0b111 -> -- quad sized player
                if o < 32
                    then testBit bitmap (flipIf reflect $ (fromIntegral ((o `shift` (-2)) .&. 7)))
                    else False

-- StateAtari programmer's guide p.40
{- INLINE player0 -}
player0 :: IOUArray OReg Word8 -> Graphics -> CInt -> CInt -> IO Bool
player0 r graphics' hpos' ppos0' = do
    let o = hpos'-ppos0'
    sizeCopies <- (0b111 .&.) <$> fastGetORegister r nusiz0
    let !delayP0' = graphics' ^. delayP0
    let !grp0' = if delayP0'
        then graphics' ^.oldGrp0
        else graphics' ^. newGrp0
    refp0' <- fastGetORegister r refp0
    return $ stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: IOUArray OReg Word8 -> Graphics -> CInt -> CInt -> IO Bool
player1 r graphics' hpos' ppos1' = do
    let o = hpos'-ppos1'
    sizeCopies <- (0b111 .&.) <$> fastGetORegister r nusiz1
    let !delayP1' = graphics' ^. delayP1
    let !grp1' = if delayP1'
        then graphics' ^. oldGrp1
        else graphics' ^. newGrp1
    refp1' <- fastGetORegister r refp1
    return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

missileSize :: Word8 -> CInt
missileSize nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

-- StateAtari programmer's guide p.22
{- INLINE missile0 -}
-- XXX Note that this updates mpos0 so need to take into account XXX
missile0 :: IOUArray OReg Word8 -> CInt -> CInt -> Word8 -> IO Bool
missile0 r hpos' mpos0' resmp0' = do
    enam0' <- fastGetORegister r enam0
    if testBit resmp0' 1
        then return False
        else if testBit enam0' 1
            then do
                let o = hpos'-mpos0'
                nusiz0' <- fastGetORegister r nusiz0
                return $ o >= 0 && o < missileSize nusiz0'
            else return False


{- INLINE missile1 -}
missile1 :: IOUArray OReg Word8 -> CInt -> CInt -> Word8 -> IO Bool
missile1 r hpos' mpos1' resmp1' = do
    enam1' <- fastGetORegister r enam1
    if (testBit resmp1' 1)
        then return False
        else if testBit enam1' 1
            then do
                let o = hpos'-mpos1'
                nusiz1' <- fastGetORegister r nusiz1
                return $ o >= 0 && o < missileSize nusiz1'
            else return False

{- INLINE ball -}
ball :: Graphics -> Word8 -> CInt -> CInt -> IO Bool
ball graphics' ctrlpf' hpos' bpos' = do
    let delayBall' = graphics' ^. delayBall
    let enabl' = if delayBall'
        then graphics' ^. oldBall
        else graphics' ^. newBall
    if enabl'
        then do
            let o = hpos'-bpos'
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < ballSize
        else return False

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, 192)

{-# INLINE clockMove #-}
clockMove :: Word8 -> CInt
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

{- INLINE stellaHmclr -}
stellaHmclr :: (MonadIO m, MonadState StateAtari m) => m ()
stellaHmclr = do
    putORegister hmp0 0
    putORegister hmp1 0
    putORegister hmm0 0
    putORegister hmm1 0
    putORegister hmbl 0

{- INLINE stellaCxclr -}
stellaCxclr :: (MonadIO m, MonadState StateAtari m) => m ()
stellaCxclr = do
    putIRegister cxm0p 0
    putIRegister cxm1p 0
    putIRegister cxm0fb 0
    putIRegister cxm1fb 0
    putIRegister cxp0fb 0
    putIRegister cxp1fb 0
    putIRegister cxblpf 0
    putIRegister cxppmm 0

{-# INLINE wrap160 #-}
wrap160 :: CInt -> CInt
wrap160 i | i>=picx && i < picx+160 = i
          | i < picx = wrap160 (i+160)
          | i >= picx+160 = wrap160 (i-160)

{- INLINE stellaHmove -}
stellaHmove :: (MonadIO m, MonadState StateAtari m) => m ()
stellaHmove = do
    poffset0 <- getORegister hmp0
    ppos0' <- use ppos0
    ppos0 .= wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- getORegister hmp1
    ppos1' <- use ppos1
    ppos1 .= wrap160 (ppos1'-clockMove poffset1)

    moffset0 <- getORegister hmm0
    mpos0' <- use mpos0
    mpos0 .= wrap160 (mpos0'-clockMove moffset0) -- XXX do rest

    moffset1 <- getORegister hmm1
    mpos1' <- use mpos1
    mpos1 .= wrap160 (mpos1'-clockMove moffset1) -- XXX do rest

    boffset <- getORegister hmbl
    bpos' <- use bpos
    bpos .= wrap160 (bpos'-clockMove boffset)

{- INLINE stellaResmp0 -}
stellaResmp0 :: (MonadIO m, MonadState StateAtari m) => m ()
stellaResmp0 = use ppos0 >>= (mpos0 .=) -- XXX

{- INLINE stellaResmp1 -}
stellaResmp1 :: MonadAtari ()
stellaResmp1 = use ppos1 >>= (mpos1 .=) -- XXX

{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- use hpos
    --stellaTick (233-fromIntegral hpos') -- 228
    --stellaTick (232-fromIntegral hpos') -- 228
    stellaTick (228-fromIntegral hpos') 

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

xscale, yscale :: CInt
xscale = 5
yscale = 3

renderDisplay :: MonadAtari ()
renderDisplay = do
    backSurface' <- use backSurface
    frontSurface' <- use frontSurface
    window' <- use frontWindow
    liftIO $ unlockSurface backSurface'
    liftIO $ SDL.surfaceBlitScaled backSurface' Nothing frontSurface'
                (Just (Rectangle (P (V2 0 0))
                    (V2 (screenWidth*xscale) (screenHeight*yscale))))
    liftIO $ lockSurface backSurface'
    liftIO $ SDL.updateWindowSurface window'

{- INLINE stellaVsync -}
stellaVsync :: Word8 -> MonadAtari ()
stellaVsync v = do
    stellaDebugStrLn 0 $ "VSYNC " ++ showHex v ""
    oldv <- getORegister vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
            hpos .= 0
            vpos .= 0
    putORegister vsync v
    renderDisplay

{- INLINE stellaVblank -}
stellaVblank :: Word8 -> MonadAtari ()
stellaVblank v = do
    stellaDebugStrLn 0 $ "VBLANK " ++ showHex v ""
    vold <- getORegister vblank
    --vold <- use vblank
    -- Set latches for INPT4 and INPT5
    when (testBit v 6) $ do
        i <- getIRegister inpt4 -- XXX write modifyIRegister
        putIRegister inpt4 (setBit i 7)
        i <- getIRegister inpt5
        putIRegister inpt5 (setBit i 7)

    --vblank .= v
    putORegister vblank v

picy :: CInt
picy = 40
picx :: CInt
picx = 68

{-# INLINE bit #-}
bit :: Int -> Bool -> Word8
bit n t = if t then 1 `shift` n else 0

{- INLINE compositeAndCollide -}
compositeAndCollide :: StateAtari -> CInt -> CInt -> IOUArray OReg Word8 -> IO Word8
compositeAndCollide stella x hpos' r = do
    resmp0' <- fastGetORegister r resmp0
    resmp1' <- fastGetORegister r resmp1

    let ir = stella ^. iregisters
    !ctrlpf' <- fastGetORegister r ctrlpf
    !colupf' <- fastGetORegister r colupf
    !colup0' <- fastGetORegister r colup0
    !colup1' <- fastGetORegister r colup1
    !colubk' <- fastGetORegister r colubk
    let !playfieldColour = if testBit ctrlpf' 1
            then if x < 80
                then colup0'
                else colup1'
            else colupf' -- does ball get this too?

    let graphics' = stella ^. graphics
    let sprites' = stella ^. sprites
    -- XXX Side effects in missile0/1
    !lmissile0 <- missile0 r hpos' (sprites' ^. s_mpos0) resmp0'
    !lmissile1 <- missile1 r hpos' (sprites' ^. s_mpos1) resmp1'
    !lplayer0 <- player0 r graphics' hpos' (sprites' ^. s_ppos0)
    !lplayer1 <- player1 r graphics' hpos' (sprites' ^. s_ppos1)
    !lball <- ball graphics' ctrlpf' hpos' (sprites' ^. s_bpos)
    !lplayfield <- playfield r ctrlpf' (fromIntegral $ x `div` 4)

    let !playball = bit 7 lplayfield .|. bit 6 lball

    when lmissile0 $ do
        fastOrIRegister ir cxm0p $ bit 7 lplayer1 .|. bit 6 lplayer0
        fastOrIRegister ir cxm0fb playball
        fastOrIRegister ir cxppmm $ bit 6 lmissile1
    when lmissile1 $ do
        fastOrIRegister ir cxm1p $ bit 7 lplayer0 .|. bit 6 lplayer1
        fastOrIRegister ir cxm1fb playball
    when lplayer0 $ do
        fastOrIRegister ir cxp0fb playball
        fastOrIRegister ir cxppmm $ bit 7 lplayer1
    when lplayer1 $ fastOrIRegister ir cxp1fb playball
    when lball $ fastOrIRegister ir cxblpf $ bit 7 lplayfield

    return $ if testBit ctrlpf' 2
                then if lball
                    then colupf'
                    else if lplayfield
                        then playfieldColour
                        else if lmissile0 || lplayer0
                            then colup0'
                            else if lmissile1 || lplayer1
                                then colup1'
                                else colubk'
                else if lmissile0 || lplayer0
                    then colup0'
                    else if lmissile1 || lplayer1
                        then colup1'
                        else if lplayfield
                            then playfieldColour
                            else if lball
                                then colupf'
                                else colubk'

{-# INLINABLE timerTick #-}
timerTick :: IntervalTimer -> IntervalTimer
timerTick timer =
    let !subtimer' = timer ^. subtimer
        !subtimer'' = subtimer'-1
    in if subtimer' /= 0
        then timer & subtimer .~ subtimer''
        else
            let !intim' = timer ^. intim
                !intim'' = intim'-1
                !interval' = timer ^. interval
            in if intim' /= 0
                then timer & intim .~ intim'' & subtimer .~ (3*interval'-1) 
                else IntervalTimer intim'' (3*1-1) 1

{-# INLINABLE updatePos #-}
updatePos :: (CInt, CInt) -> (CInt, CInt)
updatePos (hpos, vpos) =
    let hpos' = hpos+1
    in if hpos' < picx+160
        then (hpos', vpos)
        else let vpos' = vpos+1
             in if vpos' < picy+192
                then (0, vpos')
                else (0, 0)

stellaTick :: Int -> MonadAtari ()
stellaTick 0 = return ()
stellaTick n = do
    stella <- get
    let (xbreak', ybreak') = stella ^. stellaDebug . posbreak
    let (hpos', vpos') = stella ^. position
    when ((hpos', vpos') == (xbreak', ybreak')) $ do
        dumpStella
        stellaDebug . posbreak .= (-1, -1) -- Maybe maybe

    nowClock += 1
    intervalTimer %= timerTick
    
    -- Display
    when (vpos' >= picy && vpos' < picy+192 && hpos' >= picx) $ do
        let !surface = stella ^. backSurface
        !ptr <- liftIO $ surfacePixels surface
        let !ptr' = castPtr ptr :: Ptr Word32
        let !x = hpos'-picx
        let !y = vpos'-picy
        let !i = screenWidth*y+x

        stella <- get

        let r = stella ^. oregisters
        let hpos' = stella ^. hpos
        resmp0' <- liftIO $ fastGetORegister r resmp0
        resmp1' <- liftIO $ fastGetORegister r resmp1
        when (testBit resmp0' 1) $ mpos0 .= hpos'
        when (testBit resmp1' 1) $ mpos1 .= hpos'

        liftIO $ do
            !final <- compositeAndCollide stella x hpos' r
            pokeElemOff ptr' (fromIntegral i) (lut!(final `shift` (-1)))

    position %= updatePos

    stellaTick (n-1)

newtype MonadAtari a = M { unM :: StateT StateAtari IO a }
    deriving (Functor, Applicative, Monad, MonadState StateAtari, MonadIO)

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr' =
        let addr = addr' .&. 0b1111111111111 in -- 6507
            if addr >= 0x1000
            then do
                m <- use mem
                liftIO $ readArray m (iz addr)
            else if isRAM addr
                then do
                    m <- use mem
                    liftIO $ readArray m (iz addr .&. 0xff)
                else if isTIA addr
                        then readStella (addr .&. 0x3f)
                        else if isRIOT addr
                            then readStella (0x280+(addr .&. 0x1f))
                            else error $ "Mystery read from " ++ showHex addr ""


    {-# INLINE writeMemory #-}
    writeMemory addr' v =
        let addr = addr' .&. 0b1111111111111 in -- 6507
        if addr >= 0x1000
            then do
                m <- use mem
                liftIO $ writeArray m (iz addr) v
            else if isRAM addr
                then do
                    m <- use mem
                    liftIO $ writeArray m (iz addr .&. 0xff) v
                else if isTIA addr
                    then writeStella (addr .&. 0x3f) v
                    else if isRIOT addr
                            then writeStella (0x280+(addr .&. 0x1f)) v
                            else error $ "Mystery write to " ++ showHex addr ""

    {-# INLINE getPC #-}
    getPC = use (regs . pc)
    {-# INLINE tick #-}
    tick n = do
        clock += n
        stellaTick (3*n)
    {-# INLINE putC #-}
    putC b = regs . flagC .= b
    {-# INLINE getC #-}
    getC = use (regs . flagC)
    {-# INLINE putZ #-}
    putZ b = regs . flagZ .= b
    {-# INLINE getZ #-}
    getZ = use (regs . flagZ)
    {-# INLINE putI #-}
    putI b = regs . flagI .= b
    {-# INLINE getI #-}
    getI = use (regs . flagI)
    {-# INLINE putD #-}
    putD b = regs . flagD .= b
    {-# INLINE getD #-}
    getD = use (regs . flagD)
    {-# INLINE putB #-}
    putB b = regs . flagB .= b
    {-# INLINE getB #-}
    getB = use (regs . flagB)
    {-# INLINE putV #-}
    putV b = regs . flagV .= b
    {-# INLINE getV #-}
    getV = use (regs . flagV)
    {-# INLINE putN #-}
    putN b = regs . flagN .= b
    {-# INLINE getN #-}
    getN = use (regs . flagN)
    {-# INLINE getA #-}
    getA = use (regs . a)
    {-# INLINE putA #-}
    putA r = regs . a .= r
    {-# INLINE getS #-}
    getS = use (regs . s)
    {-# INLINE putS #-}
    putS r = regs . s .= r
    {-# INLINE getX #-}
    getX = use (regs . x)
    {-# INLINE putX #-}
    putX r = regs . x .= r
    {-# INLINE getP #-}
    getP = use (regs . p)
    {-# INLINE putP #-}
    putP r = regs . p .= r
    {-# INLINE getY #-}
    getY = use (regs . y)
    {-# INLINE putY #-}
    putY r = regs . y .= r
    {-# INLINE putPC #-}
    putPC r = regs . pc .= r
    {-# INLINE addPC #-}
    addPC n = regs . pc += fromIntegral n

    {- INLINE debugStr 9 -}
    debugStr n str = do
        d <- use debug
        if n <= d
            then liftIO $ putStr str
            else return ()

    {- INLINE debugStrLn 9 -}
    debugStrLn n str = do
        d <- use debug
        if n <= d
            then liftIO $ putStrLn str
            else return ()

    {- INLINE illegal -}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

dumpStella :: MonadAtari ()
dumpStella = do
    dumpMemory
    liftIO $ putStrLn "--------"
    hpos' <- use hpos
    vpos' <- use vpos
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    grp0' <- use (graphics . oldGrp0) -- XXX
    grp1' <- use (graphics . oldGrp1) -- XXX
    liftIO $ putStrLn $ "GRP0 = " ++ showHex grp0' "" ++ "(" ++ inBinary 8 grp0' ++ ")"
    liftIO $ putStrLn $ "GRP1 = " ++ showHex grp1' "" ++ "(" ++ inBinary 8 grp1' ++ ")"
    pf0' <- getORegister pf0
    pf1' <- getORegister pf1
    pf2' <- getORegister pf2
    liftIO $ putStrLn $ "PF = " ++ reverse (inBinary 4 (pf0' `shift` (-4)))
                                ++ inBinary 8 pf1'
                                ++ reverse (inBinary 8 pf2')
    nusiz0' <- getORegister nusiz0
    nusiz1' <- getORegister nusiz1
    liftIO $ putStrLn $ "NUSIZ0 = " ++ showHex nusiz0' "" ++ "(" ++ explainNusiz nusiz0' ++
                        ") NUSIZ1 = " ++ showHex nusiz1' "" ++ "(" ++ explainNusiz nusiz1' ++ ")"
    enam0' <- getORegister enam0
    enam1' <- getORegister enam1
    enablOld <- use (graphics . oldBall)
    enablNew <- use (graphics . newBall)
    liftIO $ putStr $ "ENAM0 = " ++ show (testBit enam0' 1)
    liftIO $ putStr $ " ENAM1 = " ++ show (testBit enam1' 1)
    liftIO $ putStrLn $ " ENABL = " ++ show (enablOld, enablNew)
    mpos0' <- use mpos0
    mpos1' <- use mpos1
    hmm0' <- getORegister hmm0
    hmm1' <- getORegister hmm1
    liftIO $ putStr $ "missile0 @ " ++ show mpos0' ++ "(" ++ show (clockMove hmm0') ++ ")"
    liftIO $ putStrLn $ " missile1 @ " ++ show mpos1' ++ "(" ++ show (clockMove hmm1') ++ ")"
    vdelp0' <- use (graphics . delayP0)
    vdelp1' <- use (graphics . delayP1)
    vdelbl' <- use (graphics . delayBall)
    liftIO $ putStrLn $ "VDELP0 = " ++ show vdelp0' ++ " " ++
                        "VDELP1 = " ++ show vdelp1' ++ " " ++
                        "VDELBL = " ++ show vdelbl'

{-# INLINABLE dumpMemory #-}
dumpMemory :: MonadAtari ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    liftIO $ putStr $ "(PC) = "
    liftIO $ putStr $ showHex b0 "" ++ " "
    liftIO $ putStr $ showHex b1 "" ++ " "
    liftIO $ putStrLn $ showHex b2 ""
    let (_, mne, _) = disasm regPC [b0, b1, b2]
    liftIO $ putStrLn $ mne

{-# INLINABLE dumpRegisters #-}
dumpRegisters :: MonadAtari ()
dumpRegisters = do
    -- XXX bring clock back
    --tClock <- use clock
    --putStr 9 $ "clock = " ++ show tClock
    regPC <- getPC
    liftIO $ putStr $ " pc = " ++ showHex regPC ""
    regP <- getP
    liftIO $ putStr $ " flags = " ++ showHex regP ""
    liftIO $ putStr $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
    liftIO $ putStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
    liftIO $ putStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
    liftIO $ putStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
    liftIO $ putStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
    liftIO $ putStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
    liftIO $ putStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    liftIO $ putStr $ ") A = " ++ showHex regA ""
    regX <- getX
    liftIO $ putStr $ " X = " ++ showHex regX ""
    regY <- getY
    liftIO $ putStrLn $ " Y = " ++ showHex regY ""
    regS <- getS
    liftIO $ putStrLn $ " N = " ++ showHex regS ""

{-# INLINABLE dumpState #-}
dumpState :: MonadAtari ()
dumpState = do
    dumpMemory
    dumpRegisters

--  XXX Do this! If reset occurs during horizontal blank, the object will appear at the left side of the television screen

{- INLINE setBreak -}
setBreak :: (MonadIO m, MonadState StateAtari m) =>
               CInt -> CInt -> m ()
setBreak x y = stellaDebug . posbreak .= (x+picx, y+picy)

{-
{-# INLINE usingStella #-}
usingStella :: StateT StateAtari IO a -> MonadAtari a
usingStella m = do
    stella' <- use stella
    (a, stella'') <- liftIO $ flip runStateT stella' m
    stella .= stella''
    return a
    -}
usingStella = M

{- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = 
    case addr of
       0x00 -> stellaVsync v             -- VSYNC
       0x01 -> stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> putORegister nusiz0 v        -- NUSIZ0
       0x05 -> putORegister nusiz1 v        -- NUSIZ1
       0x06 -> putORegister colup0 v               -- COLUP0
       0x07 -> putORegister colup1 v               -- COLUP1
       0x08 -> putORegister colupf v               -- COLUPF
       0x09 -> putORegister colubk v               -- COLUBK
       0x0a -> putORegister ctrlpf v               -- COLUPF
       0x0b -> putORegister refp0 v               -- REFP0
       0x0c -> putORegister refp1 v               -- REFP1
       0x0d -> putORegister pf0 v                  -- PF0
       0x0e -> putORegister pf1 v                  -- PF1
       0x0f -> putORegister pf2 v                  -- PF2
       0x10 -> use hpos >>= ((ppos0 .=) . (+5))   -- RESP0 XXX FUDGE FACTORS
       0x11 -> use hpos >>= ((ppos1 .=) . (+5))   -- RESP1
       0x12 -> use hpos >>= (mpos0 .=) . (+4)   -- RESM0
       0x13 -> use hpos >>= (mpos1 .=) . (+4)   -- RESM1
       0x14 -> use hpos >>= (bpos .=) . (+4)    -- RESBL
       0x1b -> do -- GRP0
                graphics . newGrp0 .= v
                use (graphics . newGrp1) >>= (graphics . oldGrp1 .=)
       0x1c -> do -- GRP1
                graphics . newGrp1 .= v
                use (graphics . newGrp0) >>= (graphics . oldGrp0 .=)
                use (graphics . newBall) >>= (graphics . oldBall .=)
       0x1d -> putORegister enam0 v                -- ENAM0
       0x1e -> putORegister enam1 v                -- ENAM1
       0x1f -> graphics . newBall .= testBit v 1   -- ENABL
       0x20 -> putORegister hmp0 v                 -- HMP0
       0x21 -> putORegister hmp1 v                 -- HMP1
       0x22 -> putORegister hmm0 v                 -- HMM0
       0x23 -> putORegister hmm1 v                 -- HMM1
       0x24 -> putORegister hmbl v                 -- HMBL
       0x25 -> graphics . delayP0 .= testBit v 0   -- VDELP0
       0x26 -> graphics . delayP1 .= testBit v 0   -- VDELP1
       0x27 -> graphics . delayBall .= testBit v 0   -- VDELBL
       0x28 -> putORegister resmp0 v
       0x29 -> putORegister resmp1 v
       0x2a -> stellaHmove               -- HMOVE
       0x2b -> stellaHmclr               -- HMCLR
       0x2c -> stellaCxclr               -- CXCLR
       -- XXX rewrite properly
       0x294 -> do                       -- TIM1T
        intervalTimer . interval .= 1
        intervalTimer . subtimer .= 1*3-1
        intervalTimer . intim .= v
       0x295 -> do                       -- TIM8T
        intervalTimer . interval .= 8
        intervalTimer . subtimer .= 8*3-1
        intervalTimer . intim .= v
       0x296 -> do                       -- TIM64T
        intervalTimer . interval .= 64
        intervalTimer . subtimer .= 64*3-1
        intervalTimer . intim .= v
       0x297 -> do                       -- TIM1024T
        intervalTimer . interval .= 1024
        intervalTimer . subtimer .= 1024*3-1
        intervalTimer . intim .= v
       otherwise -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

{- INLINABLE readStella -}
readStella :: Word16 -> MonadAtari Word8
readStella addr = 
    case addr of
        0x00 -> getIRegister cxm0p
        0x01 -> getIRegister cxm1p
        0x02 -> getIRegister cxp0fb
        0x03 -> getIRegister cxp1fb
        0x04 -> getIRegister cxm0fb
        0x05 -> getIRegister cxm1fb
        0x06 -> getIRegister cxblpf
        0x07 -> getIRegister cxppmm
        0x0c -> getIRegister inpt4
        0x10 -> getIRegister cxm0p
        0x11 -> getIRegister cxm1p
        0x12 -> getIRegister cxp0fb
        0x13 -> getIRegister cxp1fb
        0x14 -> getIRegister cxm0fb
        0x15 -> getIRegister cxm1fb
        0x16 -> getIRegister cxblpf
        0x17 -> getIRegister cxppmm
        0x1c -> getIRegister inpt4
        0x20 -> getIRegister cxm0p
        0x21 -> getIRegister cxm1p
        0x22 -> getIRegister cxp0fb
        0x23 -> getIRegister cxp1fb
        0x24 -> getIRegister cxm0fb
        0x25 -> getIRegister cxm1fb
        0x26 -> getIRegister cxblpf
        0x27 -> getIRegister cxppmm
        0x2c -> getIRegister inpt4
        0x30 -> getIRegister cxm0p
        0x31 -> getIRegister cxm1p
        0x32 -> getIRegister cxp0fb
        0x33 -> getIRegister cxp1fb
        0x34 -> getIRegister cxm0fb
        0x35 -> getIRegister cxm1fb
        0x36 -> getIRegister cxblpf
        0x37 -> getIRegister cxppmm
        0x3c -> getIRegister inpt4
        0x280 -> getIRegister swcha
        0x282 -> getIRegister swchb
        0x284 -> use (intervalTimer . intim)
        otherwise -> return 0 -- (liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr "") >> return 0

-- http://www.qotile.net/minidig/docs/2600_mem_map.txt

--
-- Decision tree for type of memory
--
-- testBit a 12
-- True -> ROM
-- False -> testBit a 7
--          False -> TIA
--          True -> testBit a 9
--                  True -> RIOT
--                  False -> RAM

{-# INLINE flagC #-}
flagC :: Lens' Registers Bool
flagC = p . bitAt 0

{-# INLINE flagZ #-}
flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

{-# INLINE flagI #-}
flagI :: Lens' Registers Bool
flagI = p . bitAt 2

{-# INLINE flagD #-}
flagD :: Lens' Registers Bool
flagD = p . bitAt 3

{-# INLINE flagB #-}
flagB :: Lens' Registers Bool
flagB = p . bitAt 4

{-# INLINE flagV #-}
flagV :: Lens' Registers Bool
flagV = p . bitAt 6

{-# INLINE flagN #-}
flagN :: Lens' Registers Bool
flagN = p . bitAt 7

data Args = Args { file :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin" }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

{- INLINE isPressed -}
isPressed :: InputMotion -> Bool
isPressed Pressed = True
isPressed Released = False

handleEvent :: Event -> MonadAtari ()
handleEvent event =
    case eventPayload event of
        MouseButtonEvent
            (MouseButtonEventData win Pressed device ButtonLeft clicks pos) -> do
            liftIO $ print pos
            let P (V2 x y) = pos
            usingStella $ setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)
        MouseMotionEvent
            (MouseMotionEventData win device [ButtonLeft] pos rel) -> do
            liftIO $ print pos
            let P (V2 x y) = pos
            usingStella $ setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)
        KeyboardEvent
            (KeyboardEventData win motion rep sym) -> do
            handleKey motion sym

        otherwise -> return ()

setBitTo :: Int -> Bool -> Word8 -> Word8
setBitTo i b a = if b then setBit a i else clearBit a i

comparison :: (Int -> Int -> Bool) -> Expr -> Expr -> MonadAtari Value
comparison op x y = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EBool (x `op` y)
            otherwise -> return EFail

arith :: (Int -> Int -> Int) -> Expr -> Expr -> MonadAtari Value
arith op x y = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x `op` y)
            otherwise -> return EFail

eval :: Expr -> MonadAtari Value
eval A = do
    a <- getA
    return (EInt (fromIntegral a))
eval X = do
    x <- getX
    return (EInt (fromIntegral x))
eval Y = do
    y <- getY
    return (EInt (fromIntegral y))
eval PC = do
    pc <- getPC
    return (EInt (fromIntegral pc))
eval DebugCmd.S = do
    s <- getS
    return (EInt (fromIntegral s))
eval DebugCmd.EQ = do
    z <- getZ
    return (EBool z)
eval NE = do
    z <- getZ
    return (EBool (not z))
eval CC = do
    c <- getC
    return (EBool c)
eval CS = do
    c <- getC
    return (EBool (not c))
eval PL = do
    n <- getN
    return (EBool (not n))
eval MI = do
    n <- getN
    return (EBool n)
eval DebugCmd.Clock = do
    n <- use clock
    return (EInt (fromIntegral n))
eval Row = do
    n <- use vpos
    return (EInt (fromIntegral n))
eval Col = do
    n <- use hpos
    return (EInt (fromIntegral n))

eval (Var s) = do
    v <- use (stellaDebug . variables)
    case Map.lookup s v of
        Nothing -> return EFail
        Just x -> return x

eval (Or x y) = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x .|. y)
            (EBool x, EBool y) -> return $ EBool (x || y)
            otherwise -> return EFail

eval (And x y) = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x .&. y)
            (EBool x, EBool y) -> return $ EBool (x && y)
            otherwise -> return EFail

eval (Gt x y) = comparison (>) x y
eval (Ge x y) = comparison (>=) x y
eval (Le x y) = comparison (<=) x y
eval (Eq x y) = comparison (==) x y
eval (Ne x y) = comparison (/=) x y
eval (Lt x y) = comparison (<) x y
eval (Plus x y) = arith (+) x y
eval (Times x y) = arith (*) x y
eval (Div x y) = arith div x y
eval (Minus x y) = arith (-) x y
eval (LeftShift x y) = arith (shift) x y
eval (RightShift x y) = arith (shift . negate) x y

eval (PeekByte x) = do
        x' <- eval x
        case x' of
            EInt x -> do
                y <- readMemory (fromIntegral x)
                return (EInt $ fromIntegral y)
            otherwise -> return EFail

eval (PeekWord x) = do
        x' <- eval x
        case x' of
            EInt x -> do
                lo <- readMemory (fromIntegral x)
                hi <- readMemory (fromIntegral x+1)
                return (EInt $ fromIntegral $ Core.make16 lo hi)
            otherwise -> return EFail

eval (Not x) = do
        x' <- eval x
        case x' of
            EBool x -> return $ EBool (not x)
            EInt x -> return $ EInt (-1-x)
            otherwise -> return EFail

eval (EConst x) = return (EInt x)
eval (EConstString s) = return (EString s)

eval x = do
    liftIO $ print x
    return EFail

disassemble :: Maybe Expr -> Maybe Expr -> MonadAtari ()
disassemble addr n = do
    n' <- case n of
        Just e -> do
            x' <- eval e
            case x' of
                EInt n'' -> return n''
                otherwise -> return 1
        otherwise -> return 1
    pc <- case addr of
        Just x -> do
            x' <- eval x
            case x' of
                EInt z -> return (fromIntegral z)
                otherwise -> return 0 -- error
        Nothing -> getPC
    bytes <- forM [pc..pc+3*fromIntegral n'] $ \p -> readMemory p
    liftIO $ dis n' pc bytes

execCommand :: Command -> MonadAtari Bool
execCommand cmd = 
    case cmd of
        Let var e -> do
            e' <- eval e
            stellaDebug . variables %= Map.insert var e'
            v <- use (stellaDebug . variables)
            --liftIO $ print v
            return False
        Block cmds -> do
            forM_ cmds execCommand
            return False
        DebugCmd.List addr n -> do
            disassemble addr n
            return False
        Repeat n cmd -> do
            n' <- eval n
            case n' of
                EInt n'' -> do
                    times n'' (execCommand cmd)
                otherwise -> return ()
            return False
        Cont -> do
            liftIO $ putStrLn "Continuing..."
            return True
        DumpGraphics -> dumpStella >> return False
        Step -> step >> return False
        Print es -> do
            forM_ es $ \e -> do
                val <- eval e
                liftIO $ putStr (show val)
            liftIO $ putStrLn ""
            return False
        Until cond cmd -> do
            let loop = (do
                            c <- eval cond
                            case c of
                                EBool True -> return ()
                                EBool False -> do
                                    execCommand cmd
                                    loop
                                otherwise -> do
                                    liftIO $ putStrLn "Non-boolean condition"
                                    return ())
            loop
            return False
        cmd -> do
            liftIO $ putStrLn "Unimplemented command:"
            liftIO $ print cmd
            return False

runDebugger :: MonadAtari ()
runDebugger = do
    Just line <- liftIO $ runInputT (defaultSettings { historyFile=Just ".stellarator" }) $ getInputLine "> "
    let cmd = parse parseCommand "" line
    case cmd of
        Right cmd' -> do
            q <- execCommand cmd'
            when (not q) runDebugger
        Left e -> do
            liftIO $ print e
            runDebugger

handleKey :: InputMotion -> Keysym -> MonadAtari ()
handleKey motion sym = do
    let pressed = isPressed motion
    case keysymScancode sym of
        SDL.Scancode1 -> dumpState
        SDL.ScancodeUp -> usingStella $ modifyIRegister swcha (setBitTo 4 (not pressed))
        SDL.ScancodeDown -> usingStella $ modifyIRegister swcha (setBitTo 5 (not pressed))
        SDL.ScancodeLeft -> usingStella $ modifyIRegister swcha (setBitTo 6 (not pressed))
        SDL.ScancodeRight -> usingStella $ modifyIRegister swcha (setBitTo 7 (not pressed))
        SDL.ScancodeC -> usingStella $ modifyIRegister swchb (setBitTo 1 (not pressed))
        SDL.ScancodeV -> usingStella $ modifyIRegister swchb (setBitTo 0 (not pressed))
        SDL.ScancodeSpace -> usingStella $ do
            vblank' <- getORegister vblank
            let latch = testBit vblank' 6
            case (latch, pressed) of
                (False, _) -> do
                    inpt4' <- getIRegister inpt4
                    putIRegister inpt4 ((clearBit inpt4' 7) .|. bit 7 (not pressed))
                (True, False) -> return ()
                (True, True) -> do
                    inpt4' <- getIRegister inpt4
                    putIRegister inpt4 (clearBit inpt4' 7)
        SDL.ScancodeQ -> liftIO $ exitSuccess
        SDL.ScancodeEscape -> when pressed $ do
            t <- liftIO $ forkIO $ let spin = SDL.pollEvents >> spin in spin
            dumpState
            runDebugger
            liftIO $ killThread t
        otherwise -> return ()

initState :: IOUArray Int Word8 ->
             IOUArray OReg Word8 ->
             IOUArray IReg Word8 ->
             Word16 ->
             Surface -> Surface ->
             SDL.Window -> StateAtari
initState memory oregs iregs initialPC helloWorld screenSurface window = Main.S {
      _mem = memory,  _clock = 0, _regs = R initialPC 0 0 0 0 0xff,
      _debug = 8,

      _oregisters = oregs,
      _iregisters = iregs,
      _position = (0, 0),
      _stellaSDL = StellaSDL {
          _sdlBackSurface = helloWorld,
          _sdlFrontSurface = screenSurface,
          _sdlFrontWindow = window
      },
      _sprites = Stella.Sprites.start,
      _intervalTimer = IntervalTimer {
          _intim = 0,
          _subtimer = 0,
          _interval = 0
      },
      _graphics = Stella.Graphics.start,
      _stellaClock = Main.Clock {
          _now = 0,
          _last = 0
      },
      _stellaDebug = Debug {
          _variables = Map.empty,
          _debugLevel = -1,
          _posbreak = (-1, -1)
      }
  }

main :: IO ()
main = do
    args <- cmdArgs clargs
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Stellarator" SDL.defaultWindow { SDL.windowInitialSize = V2 (xscale*screenWidth) (yscale*screenHeight) }
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window

    helloWorld <- createRGBSurface (V2 screenWidth screenHeight) RGB888

    memory <- newArray (0, 0x2000) 0 :: IO (IOUArray Int Word8)
    readBinary memory (file args) 0x1000
    pclo <- readArray memory 0x1ffc
    pchi <- readArray memory 0x1ffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)

    oregs <- newArray (0, 0x3f) 0
    --iregs <- newArray (0, 0x0d) 0
    iregs <- newArray (0, 0x300) 0 -- XXX no need for that many really
    let state = initState memory oregs iregs initialPC helloWorld screenSurface window

    let loopUntil n = do
            stellaClock' <- usingStella $ use nowClock
            when (stellaClock' < n) $ do
                step
                loopUntil n

    --SDL.setHintWithPriority SDL.NormalPriority SDL.HintRenderVSync SDL.EnableVSync
    -- https://hackage.haskell.org/package/sdl2-2.1.3

    let loop = do
            events <- liftIO $ SDL.pollEvents

            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            forM_ events handleEvent
            stellaClock' <- usingStella $ use nowClock
            loopUntil (stellaClock' + 1000)

            loop

    flip runStateT state $ unM $ do
        -- Joystick buttons not pressed
        usingStella $ do
            putIRegister inpt4 0x80
            putIRegister inpt5 0x80
            putIRegister swcha 0b11111111
            putIRegister swchb 0b00001011
        loop

    SDL.destroyWindow window
    SDL.freeSurface helloWorld
    SDL.quit
