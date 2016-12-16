{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Emulation(stellaDebug,
                 dumpStella,
                 hpos,
                 vpos,
                 dumpRegisters,
                 dumpState,
                 setBreak,
                 clock,
                 stellaClock,
                 initState,
                 bit,
                 getIRegister,
                 putIRegister,
                 trigger1,
                 modifyIRegister,
                 getORegister) where

--import Data.Monoid
--import Debug.Trace
import Control.Lens
import Control.Monad.State.Strict
import Core
import Data.Data
import Data.Array.IO
import Data.Array.Unboxed
import Metrics
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Memory
import Data.Int
import Atari2600
import Data.Word
import DebugState
import Disasm
import Foreign.C.Types
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
import BitManips
import qualified SDL

initState :: IOUArray Int Word8 ->
             BankMode ->
             IOUArray Int Word8 ->
             IOUArray OReg Word8 ->
             IOUArray IReg Word8 ->
             Word16 ->
             SDL.Surface -> SDL.Surface ->
             SDL.Window -> Atari2600
initState ram' mode rom' oregs iregs initialPC
          helloWorld screenSurface window = Atari2600 {
              _hardware = Hardware {
                  _oregisters = oregs,
                  _iregisters = iregs,
                  _position = (0, 0),
                  _stellaSDL = SDLState {
                      _sdlBackSurface = helloWorld,
                      _sdlFrontSurface = screenSurface,
                      _sdlFrontWindow = window
                  },
                  _sprites = Stella.Sprites.start,
                  _intervalTimer = Stella.IntervalTimer.start,
                  _graphics = Stella.Graphics.start,
                  _stellaClock = 0,
                  _stellaDebug = DebugState.start,
                  _trigger1 = False,
                  _pf = 0
              },
              _memory = Memory {
                  _rom = rom',
                  _ram = ram',
                  _bankMode = mode,
                  _bankOffset = 0
              },
              _clock = 0,
              _regs = R initialPC 0 0 0 0 0xff,
              _debug = 8
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
    liftIO $ mapM_ (flip (fastPutORegister r) 0) [hmp0, hmp1,
                                                  hmm0, hmm1, hmbl]

{- INLINE stellaCxclr -}
stellaCxclr :: StateT Hardware IO ()
stellaCxclr = do
    r <- use iregisters
    liftIO $ mapM_ (flip (fastPutIRegister r) 0) [cxm0p, cxm1p, cxm0fb,
                                                  cxm1fb, cxp0fb, cxp1fb,
                                                  cxblpf, cxppmm]

{- INLINE stellaHmove -}
stellaHmove :: StateT Hardware IO ()
stellaHmove = do
    Sprites !ppos0' !ppos1' !mpos0' !mpos1' !bpos' <- use sprites

    !r <- use oregisters
    let getOReg !reg = liftIO $ fastGetORegister r reg

    !poffset0 <- getOReg hmp0
    let !ppos0'' = wrap160 (ppos0'-clockMove poffset0)

    !poffset1 <- getOReg hmp1
    let !ppos1'' = wrap160 (ppos1'-clockMove poffset1)

    !moffset0 <- getOReg hmm0
    let !mpos0'' = wrap160 (mpos0'-clockMove moffset0) -- XXX do rest

    !moffset1 <- getOReg hmm1
    let !mpos1'' = wrap160 (mpos1'-clockMove moffset1) -- XXX do rest

    !boffset <- getOReg hmbl
    let !bpos'' = wrap160 (bpos'-clockMove boffset)

    sprites .= Sprites {
        _s_ppos0 = ppos0'',
        _s_ppos1 = ppos1'',
        _s_mpos0 = mpos0'',
        _s_mpos1 = mpos1'',
        _s_bpos = bpos''
    }

{- INLINE stellaResmp0 -}
stellaResmp0 ::  MonadAtari ()
stellaResmp0 = use ppos0 >>= (mpos0 .=) -- XXX

{- INLINE stellaResmp1 -}
stellaResmp1 :: MonadAtari ()
stellaResmp1 = use ppos1 >>= (mpos1 .=) -- XXX

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
    d <- use (hardware . stellaDebug . debugLevel)
    if n <= d
        then do
            liftIO $ putStr str
        else return ()

{- INLINE stellaDebugStrLn -}
stellaDebugStrLn :: Int -> String -> MonadAtari ()
stellaDebugStrLn n str = do
    d <- use (hardware . stellaDebug . debugLevel)
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
bit n t = if t then 1 `shift` n else 0

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
hpos = hardware . position . _1
vpos = hardware . position . _2

{-# INLINE ppos0 #-}
{-# INLINE ppos1 #-}
{-# INLINE mpos0 #-}
{-# INLINE mpos1 #-}
{-# INLINE bpos #-}
ppos0, ppos1, mpos0, mpos1, bpos :: Lens' Atari2600 Int
ppos0 = hardware . sprites . s_ppos0
ppos1 = hardware . sprites . s_ppos1
mpos0 = hardware . sprites . s_mpos0
mpos1 = hardware . sprites . s_mpos1
bpos = hardware . sprites . s_bpos

{-# INLINABLE updatePos #-}
updatePos :: (Int, Int) -> (Int, Int)
updatePos (hpos0, vpos0) =
    let hpos' = hpos0+1
    in if hpos' < picx+160
        then (hpos', vpos0)
        else let vpos' = vpos0+1
             in (0, vpos') -- if vpos' < picy+screenScanLines
                -- then (0, vpos')
                -- else (0, 0)
                --
makePlayfield :: MonadAtari ()
makePlayfield = do
    r <- use (hardware . oregisters)
    !pf0' <- liftIO $ fastGetORegister r pf0
    !pf1' <- liftIO $ fastGetORegister r pf1
    !pf2' <- liftIO $ fastGetORegister r pf2
    !ctrlpf' <- liftIO $ fastGetORegister r ctrlpf
    let !pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    hardware . pf .= pf'

doCollisions :: IOUArray IReg Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
doCollisions ir !lplayfield !lball !lmissile0 !lmissile1 !lplayer0 !lplayer1 = do
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


{-
    let !playfieldColour = if testBit ctrlpf' 1
            then if pixelx < 80
                then colup0
                else colup1
            else colupf -- does ball get this too? XXX
                -}

chooseColour :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> OReg
chooseColour True   _    _     True  _     _     _     _     _       = colupf
chooseColour True  True  True  False _     _     _     _     !pixelx = if pixelx < 80 then colup0 else colup1
chooseColour True  False True  False _     _     _     _     _       = colupf
chooseColour True  _     False False True  _     _     _     _       = colup0
chooseColour True  _     False False _     _     True  _     _       = colup0
chooseColour True  _     False False False True  False _     _       = colup1
chooseColour True  _     False False False _     False True  _       = colup1
chooseColour True  _     False False False False False False _       = colubk
chooseColour False _     _     _     True  _     _     _     _       = colup0
chooseColour False _     _     _     _     _     True  _     _       = colup0
chooseColour False _     _     _     False True  False _     _       = colup1
chooseColour False _     _     _     False _     False True  _       = colup1
chooseColour False True  True  _     False False False False !pixelx = if pixelx < 80 then colup0 else colup1
chooseColour False False True  _     False False False False _       = colupf
chooseColour False _     False True  False False False False _       = colupf
chooseColour False _     False False False False False False _       = colubk

{- INLINE compositeAndCollide -}
compositeAndCollide :: Hardware -> Int -> Int -> IOUArray OReg Word8 -> IO Word8
compositeAndCollide hardware'@(Hardware {_graphics = graphics',
                                         _sprites = Sprites { _s_mpos0 = !mpos0',
                                                              _s_mpos1 = !mpos1',
                                                              _s_ppos0 = !ppos0',
                                                              _s_ppos1 = !ppos1',
                                                              _s_bpos  = !bpos'
                                                            },
                                         _pf = !pf',
                                         _iregisters = ir}) !pixelx !hpos' !r = do
    let getOReg = fastGetORegister r
    !resmp0' <- getOReg resmp0
    !resmp1' <- getOReg resmp1
    !ctrlpf' <- getOReg ctrlpf
    !enam0' <- getOReg enam0
    !enam1' <- getOReg enam1
    !nusiz0' <- getOReg nusiz0
    !nusiz1' <- getOReg nusiz1

    let !lmissile0 = missile nusiz0' enam0' (hpos'-mpos0') resmp0'
    let !lmissile1 = missile nusiz1' enam1' (hpos'-mpos1') resmp1'
    !lplayer0 <- player0 r graphics' nusiz0' (hpos'-ppos0')
    !lplayer1 <- player1 r graphics' nusiz1' (hpos'-ppos1')
    let !lball = ball graphics' ctrlpf' (hpos'-bpos')
    let !playfieldx = fromIntegral (pixelx `shift` (-2))
    let !lplayfield = playfieldx >= 0 && playfieldx < 40 && testBit pf' playfieldx

    doCollisions ir lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1

    getOReg $ chooseColour (testBit ctrlpf' 2)
                           (testBit ctrlpf' 1)
                           lplayfield lball
                           lmissile0 lmissile1
                           lplayer0 lplayer1 pixelx

-- Atari2600 programmer's guide p.22
{- INLINE missile0 -}
-- XXX Note that this updates mpos0 so need to take into account XXX
missile :: Word8 -> Word8 -> Int -> Word8 -> Bool
missile _       _      o _       | o < 0                  = False
missile _       _      _ resmp0' | testBit resmp0' 1      = False
missile _       enam0' _ _       | not (testBit enam0' 1) = False
missile nusiz0' enam0' o resmp0'                          = o < missileSize nusiz0'

-- Atari2600 programmer's guide p.40
{- INLINE player0 -}
player0 :: IOUArray OReg Word8 -> Graphics -> Word8 -> Int -> IO Bool
player0 _ _ _ o | o < 0 = return False
player0 r graphics'@(Graphics {_delayP0 = !delayP0',
                               _oldGrp0 = !oldGrp0',
                               _newGrp0 = !newGrp0'}) nusiz0' o = do
    let !sizeCopies = 0b111 .&. nusiz0'
    let !grp0' = if delayP0' then oldGrp0' else newGrp0'
    refp0' <- fastGetORegister r refp0
    return $! stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: IOUArray OReg Word8 -> Graphics -> Word8 -> Int -> IO Bool
player1 _ _ _ o | o < 0 = return False
player1 r graphics'@(Graphics {_delayP1 = !delayP1',
                               _oldGrp1 = !oldGrp1',
                               _newGrp1 = !newGrp1'}) nusiz1' o = do
    let !sizeCopies = 0b111 .&. nusiz1'
    let !grp1' = if delayP1' then oldGrp1' else newGrp1'
    refp1' <- fastGetORegister r refp1
    return $! stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

{- INLINE ball -}
ball :: Graphics -> Word8 -> Int -> Bool
ball _ _ o | o < 0 = False
ball graphics' ctrlpf' o = do
    let delayBall' = graphics' ^. delayBall
    let enabl' = if delayBall'
        then graphics' ^. oldBall
        else graphics' ^. newBall
    if enabl'
        then do
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
            o >= 0 && o < ballSize
        else False

{- INLINE playfield -}
playfield :: IOUArray OReg Word8 -> Word8 -> Int -> IO Bool
playfield r ctrlpf' i | i >= 0 && i < 4 = flip testBit (i+4) <$> fastGetORegister r pf0
                      | i >=4 && i < 12 = flip testBit (11-i) <$> fastGetORegister r pf1
                      | i >= 12 && i < 20 = flip testBit (i-12) <$> fastGetORegister r pf2
playfield r ctrlpf' i | i >= 20 && i < 40 = playfield r ctrlpf' $ if testBit ctrlpf' 0 then 39-i else i-20
playfield _ _ _ = return $! False -- ???

missileSize :: Word8 -> Int
missileSize nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

{- INLINE stretchPlayer' -}
stretchPlayer' :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer' !reflect 0b000 !o !bitmap =
                o < 8 && testBit bitmap (flipIf reflect $ fromIntegral o)
stretchPlayer' !reflect 0b001 !o !bitmap =
                (o < 8 || o >= 16 && o < 24) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b010 !o !bitmap =
                (o < 8 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b011 !o !bitmap =
                (o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b100 !o !bitmap =
                (o < 8 || o >= 64) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b101 !o !bitmap =
                o < 16 && testBit bitmap (flipIf reflect $ fromIntegral ((o `shift` (-1)) .&. 7))
stretchPlayer' !reflect 0b110 !o !bitmap =
                (o < 8 || o >= 32 && o < 40 || o >= 64) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b111 !o !bitmap =
                o < 32 && testBit bitmap (flipIf reflect $ (fromIntegral ((o `shift` (-2)) .&. 7)))

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer reflect sizeCopies o bitmap =
    if o < 0 || o >= 72
        then False
        else stretchPlayer' reflect sizeCopies o bitmap

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True x = x
flipIf False x = 7-x

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
        0x284 -> use (hardware . intervalTimer . intim)
        _ -> return 0 -- (liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr "") >> return 0

{- INLINE stellaVsync -}
stellaVsync :: Word8 -> StateT Hardware IO ()
stellaVsync v = do
    or <- use oregisters
    oldv <- liftIO $ fastGetORegister or vsync
    when (testBit oldv 1 && not (testBit v 1)) $ position .= (0, 0)
    liftIO $ fastPutORegister or vsync v
    sdlState <- use stellaSDL
    liftIO $ renderDisplay sdlState

clampMissiles :: Word8 -> Word8 -> Sprites -> Sprites
clampMissiles !resmp0' !resmp1' (sprites@Sprites { _s_ppos0 = !ppos0',
                                                   _s_ppos1 = !ppos1',
                                                   _s_mpos0 = !mpos0',
                                                   _s_mpos1 = !mpos1' }) =
    let mpos0'' =  if testBit resmp0' 1 then ppos0' else mpos0'
        mpos1'' =  if testBit resmp1' 1 then ppos1' else mpos1'
    in sprites { _s_mpos0 = mpos0'', _s_mpos1 = mpos1'' }

-- Not exported
-- Writes stellaClock, intervalTimer, Position, mpos_0, mpos_1
stellaTick :: Int -> StateT Hardware IO ()
stellaTick n | n <= 0 = return ()
stellaTick n = do
    (xbreak', ybreak') <- use (stellaDebug . posbreak)
    (hpos', vpos') <- use position
    when ((hpos', vpos') == (xbreak', ybreak')) $
                    stellaDebug . posbreak .= (-1, -1) -- Maybe maybe

    -- stellaClock += 1
    -- intervalTimer %= timerTick
    
    -- Display
    when (vpos' >= picy && vpos' < picy+screenScanLines && hpos' >= picx) $ do
        !surface <- use (stellaSDL . sdlBackSurface)
        !ptr <- liftIO $ surfacePixels surface
        let !ptr' = castPtr ptr :: Ptr Word32
        let !pixelx = hpos'-picx
        let !pixely = vpos'-picy

        -- Get address of pixel in back buffer
        let !addr = fromIntegral (screenWidth*pixely+pixelx)

        r <- use oregisters
        (hpos', _) <- use position
        --resmp0' <- liftIO $ fastGetORegister r resmp0
        --resmp1' <- liftIO $ fastGetORegister r resmp1
        --zoom sprites $ clampMissiles resmp0' resmp1'

        hardware' <- get
        liftIO $ do
            !blank <- fastGetORegister r vblank
            if testBit blank 1
                then pokeElemOff ptr' addr 0x404040
                else do
                    !final <- compositeAndCollide hardware' pixelx hpos' r
                    let !rgb = lut!(final `shift` (-1))
                    pokeElemOff ptr' addr rgb

    position %= updatePos

    stellaTick (n-1)

{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- use hpos
    when (hpos' > 2) $ do
        clock += 1 -- sleep the CPU
        clock' <- use clock
        M $ zoom hardware $ stellaTickUntil (3*clock')
        stellaWsync

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

{-# INLINE church #-}
church 0 f x = x
church n f x = church (n-1) f (f x)

stellaTickUntil :: Int64 -> StateT Hardware IO ()
stellaTickUntil n = do
    !c <- use stellaClock
    let !diff = n-c
    when (diff >= 0) $ do
        -- Batch together items that don't need to be
        -- carried out on individual ticks
        stellaClock += diff
        !it <- use intervalTimer
        -- intervalTimer %= timerTick
        intervalTimer .= church diff timerTick it
        r <- use oregisters
        resmp0' <- liftIO $ fastGetORegister r resmp0
        resmp1' <- liftIO $ fastGetORegister r resmp1
        sprites %= clampMissiles resmp0' resmp1'

        stellaTick (fromIntegral diff)

{-# INLINE pureReadRom #-}
pureReadRom :: Word16 -> StateT Memory IO Word8
pureReadRom addr = do
    m <- use rom
    offset <- use bankOffset
    byte <- liftIO $ readArray m ((iz addr .&. 0xfff)+fromIntegral offset)
    return byte

{-# INLINE bankSwitch #-}
bankSwitch :: Word16 -> StateT Memory IO ()
bankSwitch addr = do
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
pureReadMemory addr =
    if addr >= 0x1000
        then M $ zoom memory $ pureReadRom addr
        else if isRAM addr
            then do
                m <- use (memory . ram)
                liftIO $ readArray m (iz addr .&. 0x7f)
            else if isTIA addr
                    then readStella (addr .&. 0x3f)
                    else if isRIOT addr
                        then readStella (0x280+(addr .&. 0x1f))
                        else error $ "The cases were exhaustive :-("

{-# INLINE pureWriteMemory #-}
pureWriteMemory :: Word16 -> Word8 -> MonadAtari ()
pureWriteMemory addr v = do
    if isRAM addr
        then do
            m <- use (memory . ram)
            liftIO $ writeArray m (iz addr .&. 0x7f) v
        else if isTIA addr
            then writeStella (addr .&. 0x3f) v
            else if isRIOT addr
                    then writeStella (0x280+(addr .&. 0x1f)) v
                    else return () -- ROM

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr' = do
        let addr = addr' .&. 0x1fff -- 6507
        byte <- pureReadMemory addr
        M $ zoom memory $ bankSwitch addr
        return byte

    {-# INLINE writeMemory #-}
    writeMemory addr' v = do
        let addr = addr' .&. 0x1fff -- 6507
        pureWriteMemory addr v
        M $ zoom memory $ bankSwitch addr

    {-# INLINE getPC #-}
    getPC = use (regs . pc)
    {-# INLINE tick #-}
    tick n = do
        clock += fromIntegral n
        c <- use clock
        M $ zoom hardware $ stellaTickUntil (3*c)
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

    {-# INLINE debugStr #-}
    debugStr _ _ = return ()
    {-# INLINE debugStrLn #-}
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
 -}           else return ()

    {- INLINE illegal -}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

dumpStella :: MonadAtari ()
dumpStella = do
    liftIO $ putStrLn "--------"
    hpos' <- use hpos
    vpos' <- use vpos
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    grp0' <- use (hardware . graphics . oldGrp0) -- XXX
    grp1' <- use (hardware . graphics . oldGrp1) -- XXX
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
    enablOld <- use (hardware . graphics . oldBall)
    enablNew <- use (hardware . graphics . newBall)
    liftIO $ putStr $ "ENAM0 = " ++ show (testBit enam0' 1)
    liftIO $ putStr $ " ENAM1 = " ++ show (testBit enam1' 1)
    liftIO $ putStrLn $ " ENABL = " ++ show (enablOld, enablNew)
    mpos0' <- use mpos0
    mpos1' <- use mpos1
    hmm0' <- getORegister hmm0
    hmm1' <- getORegister hmm1
    liftIO $ putStr $ "missile0 @ " ++ show mpos0' ++ "(" ++ show (clockMove hmm0') ++ ")"
    liftIO $ putStrLn $ " missile1 @ " ++ show mpos1' ++ "(" ++ show (clockMove hmm1') ++ ")"
    vdelp0' <- use (hardware . graphics . delayP0)
    vdelp1' <- use (hardware . graphics . delayP1)
    vdelbl' <- use (hardware . graphics . delayBall)
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
setBreak breakX breakY = hardware . stellaDebug . posbreak .= (breakX+picx, breakY+picy)

graphicsDelay :: Int64 -> MonadAtari ()
graphicsDelay n = do
    c <- use clock
    M $ zoom hardware $ stellaTickUntil (3*c+n)

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
       0x10 -> graphicsDelay 5 >> use hpos >>= (ppos0 .=)   -- RESP0
       0x11 -> graphicsDelay 5 >> use hpos >>= (ppos1 .=)   -- RESP1
       0x12 -> graphicsDelay 4 >> use hpos >>= (mpos0 .=)   -- RESM0
       0x13 -> graphicsDelay 4 >> use hpos >>= (mpos1 .=)   -- RESM1
       0x14 -> graphicsDelay 4 >> use hpos >>= (bpos .=)     -- RESBL
       0x1b -> do -- GRP0
                hardware . graphics . newGrp0 .= v
                use (hardware . graphics . newGrp1) >>= (hardware . graphics . oldGrp1 .=)
       0x1c -> do -- GRP1
                hardware . graphics . newGrp1 .= v
                use (hardware . graphics . newGrp0) >>= (hardware . graphics . oldGrp0 .=)
                use (hardware . graphics . newBall) >>= (hardware . graphics . oldBall .=)
       0x1d -> putORegister enam0 v                -- ENAM0
       0x1e -> putORegister enam1 v                -- ENAM1
       0x1f -> hardware . graphics . newBall .= testBit v 1   -- ENABL
       0x20 -> putORegister hmp0 v                 -- HMP0
       0x21 -> putORegister hmp1 v                 -- HMP1
       0x22 -> putORegister hmm0 v                 -- HMM0
       0x23 -> putORegister hmm1 v                 -- HMM1
       0x24 -> putORegister hmbl v                 -- HMBL
       0x25 -> hardware . graphics . delayP0 .= testBit v 0   -- VDELP0
       0x26 -> hardware . graphics . delayP1 .= testBit v 0   -- VDELP1
       0x27 -> hardware . graphics . delayBall .= testBit v 0   -- VDELBL
       0x28 -> putORegister resmp0 v
       0x29 -> putORegister resmp1 v
       0x2a -> M $ zoom hardware $ stellaHmove               -- HMOVE
       0x2b -> M $ zoom hardware $ stellaHmclr               -- HMCLR
       0x2c -> M $ zoom hardware $ stellaCxclr               -- CXCLR
       0x294 -> hardware . intervalTimer .= start1 v -- TIM1T
       0x295 -> hardware . intervalTimer .= start8 v -- TIM8T
       0x296 -> hardware . intervalTimer .= start64 v -- TIM64T
       0x297 -> hardware . intervalTimer .= start1024 v -- TIM1024T
       _ -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""
