{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Atari2600 where

--import Data.Monoid
--import Debug.Trace
import Control.Lens
--import Data.Lens.Strict
import Control.Monad.State.Strict
import BitManips
import Core
--import Data.Data
import Data.Array.IO
import Data.Array.Unboxed
import Metrics
--import Data.Time.Clock
import Data.Bits hiding (bit)
import Data.IORef
import Data.Bits.Lens
import Memory
import Data.Int
import Data.Word
import DebugState
import Disasm
--import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Numeric
import Prelude hiding (last)
import SDL.Video.Renderer
import Stella.Graphics
import Stella.IntervalTimer
import Stella.SDLState
import Stella.Sprites
import Stella.TIARegisters
import TIAColors
import qualified SDL

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

$(makeLenses ''Registers)

data Rendering = Rendering {
    _graphics :: Graphics,
    _sprites :: Sprites,
    _position :: (Int, Int),
    _stellaSDL :: SDLState,
    _stellaDebug :: DebugState
}

$(makeLenses ''Rendering)

data Hardware = Hardware {
    _rendering :: Rendering,
    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8,
    _stellaClock :: IORef Int64,
    _intervalTimer :: IORef IntervalTimer,
    _trigger1 :: !Bool
}

$(makeLenses ''Hardware)

data CPU = CPU {
    _memory :: Memory,
    _regs :: !Registers
}

$(makeLenses ''CPU)

data Atari2600 = Atari2600 {
    _cpu :: CPU,
    _hardware :: Hardware,
    _clock :: !Int64
    --_debug :: !Int
}

$(makeLenses ''Atari2600)

newtype MonadAtari a = M { unM :: StateT Atari2600 IO a }
    deriving (Functor, Applicative, Monad, MonadState Atari2600, MonadIO)

initState :: IOUArray Int Word8 ->
             BankMode ->
             IOUArray Int Word8 ->
             IOUArray OReg Word8 ->
             IOUArray IReg Word8 ->
             Word16 ->
             SDL.Surface -> SDL.Surface ->
             SDL.Window -> IO Atari2600
initState ram' mode rom' oregs iregs initialPC
          helloWorld screenSurface window = do
              sprites' <- Stella.Sprites.start
              intervalTimer' <- newIORef Stella.IntervalTimer.start
              stellaClock' <- newIORef 0
              return Atari2600 {
                  _hardware = Hardware {
                      _rendering = Rendering {
                          _position = (0, 0),
                          _stellaSDL = SDLState {
                              _sdlBackSurface = helloWorld,
                              _sdlFrontSurface = screenSurface,
                              _sdlFrontWindow = window
                          },
                          _graphics = Stella.Graphics.start,
                          _sprites = sprites',
                          _stellaDebug = DebugState.start
                      },
                      _oregisters = oregs,
                      _iregisters = iregs,
                      _intervalTimer = intervalTimer',
                      _stellaClock = stellaClock',
                      _trigger1 = False
                  },
                  _cpu = CPU {
                      _memory = Memory {
                          _rom = rom',
                          _ram = ram',
                          _bankMode = mode,
                          _bankOffset = 0
                      },
                      _regs = R initialPC 0 0 0 0 0xff
                  },
                  _clock = 0
              }

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

{-# INLINE putORegister #-}
putORegister :: OReg -> Word8 -> MonadAtari ()
putORegister i v = do
    r <- use (hardware . oregisters)
    liftIO $ writeArray r i v

{-# INLINE getORegister #-}
getORegister :: OReg -> MonadAtari Word8
getORegister i = do
    r <- use (hardware . oregisters)
    liftIO $ readArray r i

{-# INLINE putIRegister #-}
putIRegister :: IReg -> Word8 -> MonadAtari ()
putIRegister i v = do
    r <- use (hardware . iregisters)
    liftIO $ writeArray r i v

{-# INLINE modifyIRegister #-}
modifyIRegister :: IReg -> (Word8 -> Word8) -> MonadAtari ()
modifyIRegister i f = do
    r <- use (hardware . iregisters)
    liftIO $ (readArray r i >>= writeArray r i . f)

{-# INLINE getIRegister #-}
getIRegister :: IReg -> MonadAtari Word8
getIRegister i = do
    r <- use (hardware . iregisters)
    liftIO $ readArray r i

{-# INLINE orIRegister #-}
orIRegister :: IReg -> Word8 -> MonadAtari ()
orIRegister i v = modifyIRegister i (v .|.)

{- INLINE stellaHmclr -}
stellaHmclr :: StateT Hardware IO ()
stellaHmclr = do
    r <- use oregisters
    liftIO $ do
        fastPutORegister r hmp0 0
        fastPutORegister r hmp1 0
        fastPutORegister r hmm0 0
        fastPutORegister r hmm1 0
        fastPutORegister r hmbl 0

{- INLINE stellaCxclr -}
stellaCxclr :: StateT Hardware IO ()
stellaCxclr = do
    r <- use iregisters
    liftIO $ do
        fastPutIRegister r cxm0p 0
        fastPutIRegister r cxm1p 0
        fastPutIRegister r cxm0fb 0
        fastPutIRegister r cxm1fb 0
        fastPutIRegister r cxp0fb 0
        fastPutIRegister r cxp1fb 0
        fastPutIRegister r cxblpf 0
        fastPutIRegister r cxppmm 0

{- INLINE stellaHmove -}
stellaHmove :: StateT Hardware IO ()
stellaHmove = do
    !sprites' <- use (rendering . sprites)
    !ppos0' <- liftIO $ readArray sprites' pos_p0
    !ppos1' <- liftIO $ readArray sprites' pos_p1
    !mpos0' <- liftIO $ readArray sprites' pos_m0
    !mpos1' <- liftIO $ readArray sprites' pos_m1
    !bpos' <- liftIO $ readArray sprites' pos_b

    !r <- use oregisters
    !poffset0 <- liftIO $ fastGetORegister r hmp0
    let !ppos0'' = wrap160 (fromIntegral ppos0'-clockMove poffset0)

    !poffset1 <- liftIO $ fastGetORegister r hmp1
    let !ppos1'' = wrap160 (fromIntegral ppos1'-clockMove poffset1)

    !moffset0 <- liftIO $ fastGetORegister r hmm0
    let !mpos0'' = wrap160 (fromIntegral mpos0'-clockMove moffset0) -- XXX do rest

    !moffset1 <- liftIO $ fastGetORegister r hmm1
    let !mpos1'' = wrap160 (fromIntegral mpos1'-clockMove moffset1) -- XXX do rest

    !boffset <- liftIO $ fastGetORegister r hmbl
    let !bpos'' = wrap160 (fromIntegral bpos'-clockMove boffset)
    
    liftIO $ do
        writeArray sprites' pos_p0 (fromIntegral ppos0'')
        writeArray sprites' pos_p1 (fromIntegral ppos1'')
        writeArray sprites' pos_m0 (fromIntegral mpos0'')
        writeArray sprites' pos_m1 (fromIntegral mpos1'')
        writeArray sprites' pos_b (fromIntegral bpos'')

{- INLINE stellaResmp0 -}
stellaResmp0 :: MonadAtari ()
stellaResmp0 = do
    sprites' <- use (hardware . rendering . sprites)
    liftIO $ do
        ppos0' <- readArray sprites' pos_p0
        writeArray sprites' pos_m0 ppos0'

{- INLINE stellaResmp1 -}
stellaResmp1 :: MonadAtari ()
stellaResmp1 = do
    sprites' <- use (hardware . rendering . sprites)
    liftIO $ do
        ppos1' <- readArray sprites' pos_p1
        writeArray sprites' pos_m1 ppos1'

inBinary :: (Bits a) => Int -> a -> String
inBinary 0 _ = ""
inBinary n m = inBinary (n-1) (m `shift` (-1)) ++ if testBit m 0 then "1" else "0"

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
        _ -> error "Impossible to reach"

{- INLINE stellaDebugStr -}
stellaDebugStr :: Int -> String -> MonadAtari ()
stellaDebugStr n str = do
    d <- use (hardware . rendering . stellaDebug . debugLevel)
    if n <= d
        then do
            liftIO $ putStr str
        else return ()

{- INLINE stellaDebugStrLn -}
stellaDebugStrLn :: Int -> String -> MonadAtari ()
stellaDebugStrLn n str = do
    d <- use (hardware . rendering . stellaDebug . debugLevel)
    if n <= d
        then do
            liftIO $ putStrLn str
        else return ()

{-# INLINE wrap160 #-}
wrap160 :: Int -> Int
wrap160 i | i < picx = wrap160 (i+160)
          | i >= picx+160 = wrap160 (i-160)
wrap160 i = i

{-# INLINE clockMove #-}
clockMove :: Word8 -> Int
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

{-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

{-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

{-# INLINE iz #-}
iz :: Word16 -> Int -- or NUM
iz = fromIntegral

{-# INLINE bit #-}
bit :: Int -> Bool -> Word8
bit !n !t = if t then 1 `shift` n else 0

{-
Here's a standard kernel:
StartOfFrame
        ;--------------------------------------------------
        ; Start of vertical blank processing
        ;--------------------------------------------------
                lda #0
                sta VBLANK

                lda #2
                sta VSYNC

                sta WSYNC
                sta WSYNC
                sta WSYNC                ; 3 scanlines of VSYNC signal

                lda #0
                sta VSYNC
        ;--------------------------------------------------
        ; 37 scanlines of vertical blank...
        ;--------------------------------------------------
                ldx #0
VerticalBlank   sta WSYNC
                inx
                cpx #37
                bne VerticalBlank
        ;--------------------------------------------------
        ; Do 192 scanlines of colour-changing (our picture)
        ;--------------------------------------------------
                ldx #0                  ; this counts our scanline number
                ...
Lines           sta WSYNC
                inx
                cpx #192
                bne Lines
        ;--------------------------------------------------
        ; 30 scanlines of overscan...
        ;--------------------------------------------------
                lda #%01000010
                sta VBLANK           ; end of screen - enter blanking

                ldx #0
Overscan        sta WSYNC
                inx
                cpx #30
                bne Overscan

                jmp StartOfFrame
-}

{- INLINE stellaVblank -}
stellaVblank :: Word8 -> StateT Hardware IO ()
stellaVblank v = do
    ir <- use iregisters
    or <- use oregisters
    trigger <- use trigger1
    if not trigger
        then do
            i <- liftIO $ fastGetIRegister ir inpt4 -- XXX write modifyIRegister
            liftIO $ fastPutIRegister ir inpt4 (setBit i 7)
        else do
            i <- liftIO $ fastGetIRegister ir inpt4 -- XXX write modifyIRegister
            liftIO $ fastPutIRegister ir inpt4 (clearBit i 7)

    liftIO $ fastPutORegister or vblank v

{-# INLINE hpos #-}
{-# INLINE vpos #-}
hpos, vpos :: Lens' Atari2600 Int
hpos = hardware . rendering . position . _1
vpos = hardware . rendering . position . _2

{-
{-# INLINE ppos0 #-}
{-# INLINE ppos1 #-}
{-# INLINE mpos0 #-}
{-# INLINE mpos1 #-}
{-# INLINE bpos #-}
ppos0, ppos1, mpos0, mpos1, bpos :: Lens' Atari2600 Int
ppos0 = hardware . rendering . sprites . s_ppos0
ppos1 = hardware . rendering . sprites . s_ppos1
mpos0 = hardware . rendering . sprites . s_mpos0
mpos1 = hardware . rendering . sprites . s_mpos1
bpos = hardware . rendering . sprites . s_bpos
-}

{-# INLINABLE updatePos #-}
updatePos :: (Int, Int) -> (Int, Int)
updatePos (!hpos0, !vpos0) =
    let !hpos' = hpos0+1
    in if hpos' < picx+160
        then (hpos', vpos0)
        else let !vpos' = vpos0+1
             in (0, vpos') -- if vpos' < picy+screenScanLines
                -- then (0, vpos')
                -- else (0, 0)

makePlayfield :: MonadAtari ()
makePlayfield = do
    r <- use (hardware . oregisters)
    !pf0' <- liftIO $ fastGetORegister r pf0
    !pf1' <- liftIO $ fastGetORegister r pf1
    !pf2' <- liftIO $ fastGetORegister r pf2
    !ctrlpf' <- liftIO $ fastGetORegister r ctrlpf
    let !pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    hardware . rendering . graphics . pf .= pf'

doCollisions :: IOUArray IReg Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
doCollisions !ir !lplayfield !lmissile0 !lmissile1 !lplayer0 !lplayer1 !lball = do
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

{- INLINE compositeAndCollide -}
compositeAndCollide :: Hardware -> Int -> Int -> IOUArray OReg Word8 -> IO Word8
compositeAndCollide Hardware { _iregisters = !ir,
                               _rendering = Rendering { _graphics = !graphics'@(Graphics { _pf = pf' }),
                                                        _sprites = sprites' } }
                    !pixelx !hpos' !r = do
    ppos0' <- readArray sprites' pos_p0
    ppos1' <- readArray sprites' pos_p1
    mpos0' <- readArray sprites' pos_m0
    mpos1' <- readArray sprites' pos_m1
    bpos' <- readArray sprites' pos_b

    !resmp0' <- fastGetORegister r resmp0
    !resmp1' <- fastGetORegister r resmp1
    !ctrlpf' <- fastGetORegister r ctrlpf
    !enam0' <- fastGetORegister r enam0
    !enam1' <- fastGetORegister r enam1
    !nusiz0' <- fastGetORegister r nusiz0
    !nusiz1' <- fastGetORegister r nusiz1

    !lplayer0 <- player0 r graphics' nusiz0' (hpos'-ppos0')
    !lplayer1 <- player1 r graphics' nusiz1' (hpos'-ppos1')
    let !lmissile0 = missile nusiz0' enam0' (hpos'-mpos0') resmp0'
    let !lmissile1 = missile nusiz1' enam1' (hpos'-mpos1') resmp1'
    !lball <- ball graphics' ctrlpf' (hpos'-bpos')
    let !playfieldx = fromIntegral (pixelx `shift` (-2))
    let !lplayfield = playfieldx >= 0 && playfieldx < 40 && testBit pf' playfieldx

    doCollisions ir lplayfield lmissile0 lmissile1 lplayer0 lplayer1 lball

    {- INLINE playfieldColour -}
    let playfieldColour !pixelx !ctrlpf' = if testBit ctrlpf' 1
            then if pixelx < 80
                then fastGetORegister r colup0
                else fastGetORegister r colup1
            else fastGetORegister r colupf -- does ball get this too?

    if testBit ctrlpf' 2
        then if lball
            then fastGetORegister r colupf
            else if lplayfield
                then playfieldColour pixelx ctrlpf'
                else if lmissile0 || lplayer0
                    then fastGetORegister r colup0
                    else if lmissile1 || lplayer1
                        then fastGetORegister r colup1
                        else fastGetORegister r colubk
        else if lmissile0 || lplayer0
            then fastGetORegister r colup0
            else if lmissile1 || lplayer1
                then fastGetORegister r colup1
                else if lplayfield
                    then playfieldColour pixelx ctrlpf'
                    else if lball
                        then fastGetORegister r colupf
                        else fastGetORegister r colubk

-- Atari2600 programmer's guide p.22
{- INLINE missile0 -}
-- XXX Note that this updates mpos0 so need to take into account XXX
-- XXX Missiles need to be replicated like players
missile :: Word8 -> Word8 -> Int -> Word8 -> Bool
missile !nusiz0' !enam0' !o !resmp0' =
    if o >= 0 && o < missileSize nusiz0'
        then if testBit resmp0' 1
                then False
                else testBit enam0' 1
        else False

-- Atari2600 programmer's guide p.40
{- INLINE player0 -}
player0 :: IOUArray OReg Word8 -> Graphics ->
           Word8 -> Int -> IO Bool
player0 r Graphics { _delayP0 = !delayP0', _oldGrp0 = !oldGrp0', _newGrp0 = !newGrp0' } !nusiz0' !o =
    if o < 0 || o >= 72
        then return False
        else do
            let !sizeCopies = 0b111 .&. nusiz0'
            let !grp0' = if delayP0' then oldGrp0' else newGrp0'
            !refp0' <- fastGetORegister r refp0
            return $ stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: IOUArray OReg Word8 -> Graphics ->
           Word8 -> Int -> IO Bool
player1 r Graphics { _delayP1 = !delayP1', _oldGrp1 = !oldGrp1', _newGrp1 = !newGrp1' } !nusiz1' !o = 
    if o < 0 || o >= 72
        then return False
        else do
            let !sizeCopies = 0b111 .&. nusiz1'
            let !grp1' = if delayP1' then oldGrp1' else newGrp1'
            !refp1' <- fastGetORegister r refp1
            return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

{- INLINE ball -}
ball :: Graphics -> Word8 -> Int -> IO Bool
ball Graphics { _oldBall = oldBall', _newBall = newBall', _delayBall = delayBall' } !ctrlpf' !o = do
    if o < 0 || o >= 8 then
        return False
        else do
            let !enabl' = if delayBall' then oldBall' else newBall'
            if enabl'
                then do
                    let !ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
                    return $ o < ballSize
                else return False

{-
{- INLINE playfield -}
playfield :: IOUArray OReg Word8 -> Word8 -> Int -> IO Bool
playfield !r _ !i        | i >= 0 && i < 4 = flip testBit (i+4) <$> fastGetORegister r pf0
                         | i >=4 && i < 12 = flip testBit (11-i) <$> fastGetORegister r pf1
                         | i >= 12 && i < 20 = flip testBit (i-12) <$> fastGetORegister r pf2
playfield !r !ctrlpf' !i | i >= 20 && i < 40 = playfield r ctrlpf' $ if testBit ctrlpf' 0 then 39-i else i-20
playfield _ _ _ = return False -- ???
-}

missileSize :: Word8 -> Int
missileSize !nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer !reflect !sizeCopies !o !bitmap =
    if o < 0 || o >= 72
        then False
        else case sizeCopies of
            0b000 -> -- one copy
                o < 8 && testBit bitmap (flipIf reflect $ fromIntegral o)
            0b001 -> -- two copies close
                (o < 8 || o >= 16 && o < 24) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
            0b010 -> -- two copies - med
                (o < 8 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
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
            _ -> error "Impossible"

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True !x = x
flipIf False !x = 7-x

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
        0x0d -> getIRegister inpt5
        0x10 -> getIRegister cxm0p
        0x11 -> getIRegister cxm1p
        0x12 -> getIRegister cxp0fb
        0x13 -> getIRegister cxp1fb
        0x14 -> getIRegister cxm0fb
        0x15 -> getIRegister cxm1fb
        0x16 -> getIRegister cxblpf
        0x17 -> getIRegister cxppmm
        0x1c -> getIRegister inpt4
        0x1d -> getIRegister inpt5
        0x20 -> getIRegister cxm0p
        0x21 -> getIRegister cxm1p
        0x22 -> getIRegister cxp0fb
        0x23 -> getIRegister cxp1fb
        0x24 -> getIRegister cxm0fb
        0x25 -> getIRegister cxm1fb
        0x26 -> getIRegister cxblpf
        0x27 -> getIRegister cxppmm
        0x2c -> getIRegister inpt4
        0x2d -> getIRegister inpt5
        0x30 -> getIRegister cxm0p
        0x31 -> getIRegister cxm1p
        0x32 -> getIRegister cxp0fb
        0x33 -> getIRegister cxp1fb
        0x34 -> getIRegister cxm0fb
        0x35 -> getIRegister cxm1fb
        0x36 -> getIRegister cxblpf
        0x37 -> getIRegister cxppmm
        0x3c -> getIRegister inpt4
        0x3d -> getIRegister inpt5
        0x280 -> getIRegister swcha
        0x282 -> getIRegister swchb
        0x284 -> do
                    pIntervalTimer <- use (hardware . intervalTimer)
                    intervalTimer' <- liftIO $ readIORef pIntervalTimer
                    return (intervalTimer' ^. intim)
        _ -> return 0 -- (liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr "") >> return 0

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/
--
{- INLINE stellaVsync -}
stellaVsync :: Word8 -> StateT Hardware IO ()
stellaVsync v = do
    or <- use oregisters
    oldv <- liftIO $ fastGetORegister or vsync
    when (testBit oldv 1 && not (testBit v 1)) $ rendering . position .= (0, 0)
    liftIO $ fastPutORegister or vsync v
    sdlState <- use (rendering . stellaSDL)
    liftIO $ renderDisplay sdlState

renderPixel :: IOUArray OReg Word8 -> Hardware -> Int -> Int -> IO ()
renderPixel r hardware'@(Hardware { _rendering = Rendering {
                                        _stellaSDL = SDLState { _sdlBackSurface = !surface }
                                    }
                                  }) !hpos' !vpos' = do
    !ptr <- liftIO $ surfacePixels surface
    let !ptr' = castPtr ptr :: Ptr Word32
    let !pixelx = hpos'-picx
    let !pixely = vpos'-picy

    -- Get address of pixel in back buffer
    let !i = screenWidth*pixely+pixelx

    liftIO $ do
        !final <- compositeAndCollide hardware' pixelx hpos' r
        let !rgb = lut!(final `shift` (-1))
        let !grayedOut = ((rgb .&. 0xfefefe) `shift` (-1))+0x202020
        !blank <- fastGetORegister r vblank
        pokeElemOff ptr' (fromIntegral i) (if testBit blank 1 then grayedOut else rgb)

lockMissilesToPlayers :: Word8 -> Word8 -> Sprites -> IO ()
lockMissilesToPlayers resmp0' resmp1' sprites' = do
    when (testBit resmp0' 1) $ do
        ppos0' <- readArray sprites' pos_p0
        writeArray sprites' pos_m0 ppos0'
    when (testBit resmp1' 1) $ do
        ppos1' <- readArray sprites' pos_p1
        writeArray sprites' pos_m1 ppos1'

doRendering :: Int -> Int -> StateT Hardware IO ()
doRendering hpos' vpos' = do
    hardware'@(Hardware { _oregisters = !r, _rendering = Rendering { _sprites = !sprites' }}) <- get
    !resmp0' <- liftIO $ fastGetORegister r resmp0
    !resmp1' <- liftIO $ fastGetORegister r resmp1
    liftIO $ lockMissilesToPlayers resmp0' resmp1' sprites'
    --rendering . sprites %= lockMissilesToPlayers resmp0' resmp1'
    liftIO $ renderPixel r hardware' hpos' vpos'

stellaTick :: Int -> StateT Hardware IO ()
stellaTick n | n <= 0 = return ()
stellaTick n = do
    (!hpos', !vpos') <- use (rendering . position)

    -- Display
    when (vpos' >= picy && vpos' < picy+screenScanLines && hpos' >= picx) $
        doRendering hpos' vpos'

    rendering . position %= updatePos
    pStellaClock <- use stellaClock
    liftIO $ modifyIORef pStellaClock (+1)
    pIntervalTimer <- use intervalTimer
    liftIO $ modifyIORef pIntervalTimer timerTick

    (!xbreak', !ybreak') <- use (rendering . stellaDebug . posbreak)
    when ((hpos', vpos') == (xbreak', ybreak')) $ do
        dumpStella
        rendering . stellaDebug . posbreak .= (-1, -1) -- Maybe maybe

    stellaTick (n-1)

{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- use hpos
    when (hpos' > 2) $ do
        clock += 1 -- sleep the CPU XXX This is wrong
        !clock' <- use clock
        M $ zoom hardware $ stellaTickUntil (3*clock')
        stellaWsync

stellaTickUntil :: Int64 -> StateT Hardware IO ()
stellaTickUntil !n = do
    pStellaClock <- use stellaClock
    !c <- liftIO $ readIORef pStellaClock
    stellaTick (fromIntegral (n-c))

{-# INLINE pureReadRom #-}
pureReadRom :: Word16 -> StateT Memory IO Word8
pureReadRom !addr = do
    m <- use rom
    offset <- use bankOffset
    byte <- liftIO $ readArray m ((iz addr .&. 0xfff)+fromIntegral offset)
    return byte

{-# INLINE bankSwitch #-}
bankSwitch :: Word16 -> StateT Memory IO ()
bankSwitch !addr = do
    when (addr >= 0x1ff6) $ do
        bankType <- use bankMode
        case bankType of
            UnBanked -> return ()
            F8 -> do
                when (addr == 0x1ff8) $ bankOffset .= 0
                when (addr == 0x1ff9) $ bankOffset .= 0x1000
            F6 -> do
                when (addr == 0x1ff6) $ bankOffset .= 0
                when (addr == 0x1ff7) $ bankOffset .= 0x1000
                when (addr == 0x1ff8) $ bankOffset .= 0x2000
                when (addr == 0x1ff9) $ bankOffset .= 0x3000

{-# INLINE pureReadMemory #-}
pureReadMemory :: Word16 -> MonadAtari Word8
pureReadMemory !addr =
    if addr >= 0x1000
        then M $ zoom (cpu . memory) $ pureReadRom addr
        else if isRAM addr
            then do
                m <- use (cpu . memory . ram)
                liftIO $ readArray m (iz addr .&. 0x7f)
            else if isTIA addr
                    then readStella (addr .&. 0x3f)
                    else if isRIOT addr
                        then readStella (0x280+(addr .&. 0x1f))
                        else error $ "The cases were exhaustive :-("

{-# INLINE pureWriteMemory #-}
pureWriteMemory :: Word16 -> Word8 -> MonadAtari ()
pureWriteMemory !addr !v = do
    if isRAM addr
        then do
            m <- use (cpu . memory . ram)
            liftIO $ writeArray m (iz addr .&. 0x7f) v
        else if isTIA addr
            then writeStella (addr .&. 0x3f) v
            else if isRIOT addr
                    then writeStella (0x280+(addr .&. 0x1f)) v
                    else return () -- ROM

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory !addr' = do
        let addr = addr' .&. 0x1fff -- 6507
        byte <- pureReadMemory addr
        M $ zoom (cpu . memory) $ bankSwitch addr
        return byte

    {-# INLINE writeMemory #-}
    writeMemory !addr' !v = do
        let addr = addr' .&. 0x1fff -- 6507
        pureWriteMemory addr v
        M $ zoom (cpu . memory) $ bankSwitch addr

    {-# INLINE getPC #-}
    getPC = use (cpu . regs . pc)
    {-# INLINE tick #-}
    tick n = do
        clock += fromIntegral n
        c <- use clock
        M $ zoom hardware $ stellaTickUntil (3*c)
    {-# INLINE putC #-}
    putC b = cpu . regs . flagC .= b
    {-# INLINE getC #-}
    getC = use (cpu . regs . flagC)
    {-# INLINE putZ #-}
    putZ b = cpu . regs . flagZ .= b
    {-# INLINE getZ #-}
    getZ = use (cpu . regs . flagZ)
    {-# INLINE putI #-}
    putI b = cpu . regs . flagI .= b
    {-# INLINE getI #-}
    getI = use (cpu . regs . flagI)
    {-# INLINE putD #-}
    putD b = cpu . regs . flagD .= b
    {-# INLINE getD #-}
    getD = use (cpu . regs . flagD)
    {-# INLINE putB #-}
    putB b = cpu . regs . flagB .= b
    {-# INLINE getB #-}
    getB = use (cpu . regs . flagB)
    {-# INLINE putV #-}
    putV b = cpu . regs . flagV .= b
    {-# INLINE getV #-}
    getV = use (cpu . regs . flagV)
    {-# INLINE putN #-}
    putN b = cpu . regs . flagN .= b
    {-# INLINE getN #-}
    getN = use (cpu . regs . flagN)
    {-# INLINE getA #-}
    getA = use (cpu . regs . a)
    {-# INLINE putA #-}
    putA r = cpu . regs . a .= r
    {-# INLINE getS #-}
    getS = use (cpu . regs . s)
    {-# INLINE putS #-}
    putS r = cpu . regs . s .= r
    {-# INLINE getX #-}
    getX = use (cpu . regs . x)
    {-# INLINE putX #-}
    putX r = cpu . regs . x .= r
    {-# INLINE getP #-}
    getP = use (cpu . regs . p)
    {-# INLINE putP #-}
    putP r = cpu . regs . p .= r
    {-# INLINE getY #-}
    getY = use (cpu . regs . y)
    {-# INLINE putY #-}
    putY r = cpu . regs . y .= r
    {-# INLINE putPC #-}
    putPC r = cpu . regs . pc .= r
    {-# INLINE addPC #-}
    addPC n = cpu . regs . pc += fromIntegral n

    debugStr _ _ = return ()
    debugStrLn _ _ = return ()
{-
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
-}

    {- INLINE illegal -}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

dumpStella :: StateT Hardware IO ()
dumpStella = do
    liftIO $ putStrLn "--------"
    (hpos', vpos') <- use (rendering . position)
    r <- use oregisters
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    grp0' <- use (rendering . graphics . oldGrp0) -- XXX
    grp1' <- use (rendering . graphics . oldGrp1) -- XXX
    liftIO $ putStrLn $ "GRP0 = " ++ showHex grp0' "" ++ "(" ++ inBinary 8 grp0' ++ ")"
    liftIO $ putStrLn $ "GRP1 = " ++ showHex grp1' "" ++ "(" ++ inBinary 8 grp1' ++ ")"
    pf0' <- liftIO $ fastGetORegister r pf0
    pf1' <- liftIO $ fastGetORegister r pf1
    pf2' <- liftIO $ fastGetORegister r pf2
    liftIO $ putStrLn $ "PF = " ++ reverse (inBinary 4 (pf0' `shift` (-4)))
                                ++ inBinary 8 pf1'
                                ++ reverse (inBinary 8 pf2')
    nusiz0' <- liftIO $ fastGetORegister r nusiz0
    nusiz1' <- liftIO $ fastGetORegister r nusiz1
    liftIO $ putStrLn $ "NUSIZ0 = " ++ showHex nusiz0' "" ++ "(" ++ explainNusiz nusiz0' ++
                        ") NUSIZ1 = " ++ showHex nusiz1' "" ++ "(" ++ explainNusiz nusiz1' ++ ")"
    enam0' <- liftIO $ fastGetORegister r enam0
    enam1' <- liftIO $ fastGetORegister r enam1
    enablOld <- use (rendering . graphics . oldBall)
    enablNew <- use (rendering . graphics . newBall)
    liftIO $ putStr $ "ENAM0 = " ++ show (testBit enam0' 1)
    liftIO $ putStr $ " ENAM1 = " ++ show (testBit enam1' 1)
    liftIO $ putStrLn $ " ENABL = " ++ show (enablOld, enablNew)
    sprites' <- use (rendering . sprites)
    mpos0' <- liftIO $ readArray sprites' pos_m0
    mpos1' <- liftIO $ readArray sprites' pos_m1
    hmm0' <- liftIO $ fastGetORegister r hmm0
    hmm1' <- liftIO $ fastGetORegister r hmm1
    liftIO $ putStr $ "missile0 @ " ++ show mpos0' ++ "(" ++ show (clockMove hmm0') ++ ")"
    liftIO $ putStrLn $ " missile1 @ " ++ show mpos1' ++ "(" ++ show (clockMove hmm1') ++ ")"
    vdelp0' <- use (rendering . graphics . delayP0)
    vdelp1' <- use (rendering . graphics . delayP1)
    vdelbl' <- use (rendering . graphics . delayBall)
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

{- INLINE setBreak -}
setBreak :: Int -> Int -> MonadAtari ()
setBreak breakX breakY = hardware . rendering . stellaDebug . posbreak .= (breakX+picx, breakY+picy)

graphicsDelay :: Int64 -> MonadAtari ()
graphicsDelay n = do
    c <- use clock
    M $ zoom hardware $ stellaTickUntil (3*c+n)

setSpritePos :: SpriteCounter -> MonadAtari ()
setSpritePos sprite = do
    hpos' <- use hpos
    sprites' <- use (hardware . rendering . sprites)
    liftIO $ writeArray sprites' sprite hpos'

{- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = 
    case addr of
       0x00 -> M $ zoom hardware $ stellaVsync v             -- VSYNC
       0x01 -> M $ zoom hardware $ stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> putORegister nusiz0 v        -- NUSIZ0
       0x05 -> putORegister nusiz1 v        -- NUSIZ1
       0x06 -> putORegister colup0 v               -- COLUP0
       0x07 -> putORegister colup1 v               -- COLUP1
       0x08 -> putORegister colupf v               -- COLUPF
       0x09 -> putORegister colubk v               -- COLUBK
       0x0a -> putORegister ctrlpf v >> makePlayfield               -- COLUPF
       0x0b -> putORegister refp0 v               -- REFP0
       0x0c -> putORegister refp1 v               -- REFP1
       0x0d -> graphicsDelay 4 >> putORegister pf0 v >> makePlayfield                  -- PF0
       0x0e -> graphicsDelay 4 >> putORegister pf1 v >> makePlayfield                  -- PF1
       0x0f -> graphicsDelay 4 >> putORegister pf2 v >> makePlayfield                  -- PF2
       0x10 -> graphicsDelay 5 >> setSpritePos pos_p0   -- RESP0
       0x11 -> graphicsDelay 5 >> setSpritePos pos_p1   -- RESP1
       0x12 -> graphicsDelay 4 >> setSpritePos pos_m0   -- RESM0
       0x13 -> graphicsDelay 4 >> setSpritePos pos_m1   -- RESM1
       0x14 -> graphicsDelay 4 >> setSpritePos pos_b    -- RESB
       0x1b -> do -- GRP0
                hardware . rendering . graphics . newGrp0 .= v
                use (hardware . rendering . graphics . newGrp1) >>= (hardware . rendering . graphics . oldGrp1 .=)
       0x1c -> do -- GRP1
                hardware . rendering . graphics . newGrp1 .= v
                use (hardware . rendering . graphics . newGrp0) >>= (hardware . rendering . graphics . oldGrp0 .=)
                use (hardware . rendering . graphics . newBall) >>= (hardware . rendering . graphics . oldBall .=)
       0x1d -> putORegister enam0 v                -- ENAM0
       0x1e -> putORegister enam1 v                -- ENAM1
       0x1f -> hardware . rendering . graphics . newBall .= testBit v 1   -- ENABL
       0x20 -> putORegister hmp0 v                 -- HMP0
       0x21 -> putORegister hmp1 v                 -- HMP1
       0x22 -> putORegister hmm0 v                 -- HMM0
       0x23 -> putORegister hmm1 v                 -- HMM1
       0x24 -> putORegister hmbl v                 -- HMBL
       0x25 -> hardware . rendering . graphics . delayP0 .= testBit v 0   -- VDELP0
       0x26 -> hardware . rendering . graphics . delayP1 .= testBit v 0   -- VDELP1
       0x27 -> hardware . rendering . graphics . delayBall .= testBit v 0   -- VDELBL
       0x28 -> putORegister resmp0 v
       0x29 -> putORegister resmp1 v
       0x2a -> M $ zoom hardware $ stellaHmove               -- HMOVE
       0x2b -> M $ zoom hardware $ stellaHmclr               -- HMCLR
       0x2c -> M $ zoom hardware $ stellaCxclr               -- CXCLR
       0x294 -> M $ zoom hardware $ startIntervalTimer (start1 v) -- TIM1T
       0x295 -> M $ zoom hardware $ startIntervalTimer (start8 v) -- TIM8T
       0x296 -> M $ zoom hardware $ startIntervalTimer (start64 v) -- TIM64T
       0x297 -> M $ zoom hardware $ startIntervalTimer (start1024 v) -- TIM1024T
       _ -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

startIntervalTimer :: IntervalTimer -> StateT Hardware IO ()
startIntervalTimer start = do
    pIntervalTimer <- use intervalTimer
    liftIO $ writeIORef pIntervalTimer start
