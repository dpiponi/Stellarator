{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Emulation(stellaDebug,
                 dumpRegisters,
                 dumpState,
                 clock,
                 stellaClock,
                 initState,
                 trigger1,
                 trigger2,
                 loopUntil,
                 initHardware,
                 load) where

import Asm()
import Graphics.Rendering.OpenGL as GL
--import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
import Atari2600
import BitManips
import Display
import Control.Lens
import Control.Monad.Reader
import Core
import Data.Array.IO
import Data.Array.Unboxed as U
import Data.Bits hiding (bit)
import Data.Bits.Lens
--import Data.Array.Storable
import Data.IORef
import Data.Int
import Data.Word
import DebugState
import Disasm
import Foreign.Ptr
import Memory
import Metrics
import Numeric
import Prelude hiding (last)
import Asm
import VideoOps
import qualified SDL

timerTick' :: Word8 -> Int -> Int -> Word8 -> (Word8, Int, Int, Word8)
timerTick' 0      0         _         _       = (-1,       3*1-1,         1,         0x80)
timerTick' intim' 0         interval' timint' = (intim'-1, 3*interval'-1, interval', timint')
timerTick' intim' subtimer' interval' timint' = (intim',   subtimer'-1,   interval', timint')

timerTick :: MonadAtari ()
timerTick = do
    (intim'', subtimer'', interval'', timint'') <- timerTick' <$> load intim <*> load subtimer <*> load interval <*> load timint
    intim @= intim''
    subtimer @= subtimer''
    interval @= interval''
    timint @= timint''

startIntervalTimerN :: Int -> Word8 -> MonadAtari ()
startIntervalTimerN n v = do
    interval @= n
    subtimer @= 0 -- Was 3*n-1
    intim @= v
    timint @= 0

initState :: Int -> Int -> Int -> Int ->
             IOUArray Int Word8 ->
#if TRACE
             StorableArray Int Word8 ->
#endif
             BankState ->
             IOUArray Int Word8 ->
             Word16 ->
             SDL.Window -> 
             GL.Program ->
             GL.AttribLocation ->
             GL.TextureObject ->
             Ptr Word8 ->
             IO Atari2600
initState xscale' yscale' width height ram'
#if TRACE
            record'
#endif
            initBankState rom' initialPC window prog attrib initTex initTextureData = do
          stellaDebug' <- newIORef DebugState.start
          bankState' <- newIORef initBankState
          clock' <- newIORef 0
          -- debug' <- newIORef 8
          stellaClock' <- newIORef 0
#if TRACE
          recordPtr' <- newIORef 0
#endif
          boolArray' <- newArray (0, maxBool) False
          intArray' <- newArray (0, maxInt) 0      -- Overkill
          word64Array' <- newArray (0, maxWord64) 0
          word16Array' <- newArray (0, maxWord16) 0      -- Overkill
          word8Array' <- newArray (0, maxWord8) 0
          liftIO $ st word16Array' pc initialPC
          return $ Atari2600 {
              _xscale = xscale',
              _yscale = yscale',
              _windowWidth = width,
              _windowHeight = height,
              _rom = rom',
#if TRACE
              _record = record',
              _recordPtr = recordPtr',
#endif
              _ram = ram',
              _stellaDebug = stellaDebug',
              _bankState = bankState',
              _clock = clock',
              _stellaClock = stellaClock',
              _boolArray = boolArray',
              _intArray = intArray',
              _word64Array = word64Array',
              _word16Array = word16Array',
              _word8Array = word8Array',
              _sdlWindow = window,
              _textureData = initTextureData,
              _tex = initTex,
              _glProg = prog,
              _glAttrib = attrib,
              _delays = U.listArray (0, 0x2c) (replicate (0x2c+1) 0) U.// [
                (0x00, 0), -- VSYNC
                (0x01, 0), -- VBLANK
                (0x02, 0), -- WSYNC
                (0x03, 0), -- NUSIZ0
                (0x05, 4), -- NUSIZ1
                (0x06, 0), -- COLUP0
                (0x07, 0), -- COLUP0
                (0x08, 0), -- COLUPF
                (0x09, 0), -- COLUBK
                (0x0a, 0), -- CTRLPF
                (0x0b, 0), -- REFP0
                (0x0c, 0), -- REFP1
                (0x0d, 3), -- PF0
                (0x0e, 3), -- PF1
                (0x0f, 3), -- PF2
                (0x10, 5), -- RESP0
                (0x11, 5), -- RESP1
                (0x12, 4), -- RESM0
                (0x13, 4), -- RESM1
                (0x14, 4), -- RESBL
                (0x1b, 1), -- GRP0
                (0x1c, 1), -- GRP1
                (0x1d, 0), -- ENAM0
                (0x1e, 0), -- ENAM1
                (0x1f, 0), -- ENABL
                (0x20, 0), -- HMP0
                (0x21, 0), -- HMP1
                (0x22, 0), -- HMM0
                (0x23, 0), -- HMM1
                (0x24, 0), -- HMBL
                (0x25, 0), -- VDELP0
                (0x26, 0), -- VDELP1
                (0x27, 0), -- VDELBL
                (0x28, 0), -- RESMP0
                (0x29, 0), -- RESMP1
                (0x2a, 0), -- HMOVE
                (0x2b, 0), -- HMCLR
                (0x2c, 0)  -- CXCLR
                ]
          }

{- INLINE stellaHmclr -}
stellaHmclr :: MonadAtari ()
stellaHmclr =
    mapM_ (@= 0) [hmp0, hmp1, hmm0, hmm1, hmbl]

{- INLINE stellaCxclr -}
stellaCxclr :: MonadAtari ()
stellaCxclr =
    mapM_ (@= 0) [cxm0p, cxm1p, cxm0fb, cxm1fb, cxp0fb, cxp1fb, cxblpf, cxppmm]

{- INLINE stellaHmove -}
stellaHmove :: MonadAtari ()
stellaHmove = do
    pendingHmove @= True

    poffset0 <- load hmp0
    modify ppos0 $ \ppos0' ->  wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- load hmp1
    modify ppos1 $ \ppos1' ->  wrap160 (ppos1'-clockMove poffset1)

    moffset0 <- load hmm0
    modify mpos0 $ \mpos0' ->  wrap160 (mpos0'-clockMove moffset0)

    moffset1 <- load hmm1
    modify mpos1 $ \mpos1' ->  wrap160 (mpos1'-clockMove moffset1)

    boffset <- load hmbl
    modify bpos $ \bpos' -> wrap160 (bpos'-clockMove boffset)

{-# INLINE wrap160 #-}
wrap160 :: Int -> Int
wrap160 i | i < picx = wrap160 (i+160)
          | i >= picx+160 = wrap160 (i-160)
wrap160 i = i

{-# INLINE iz #-}
iz :: Word16 -> Int -- or NUM
iz = fromIntegral

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
stellaVblank :: Word8 -> MonadAtari ()
stellaVblank v = do
    trigger1' <- load trigger1
    modify inpt4 $ bitAt 7 .~ not trigger1'
    trigger2' <- load trigger2
    modify inpt5 $ bitAt 7 .~ not trigger2'
    vblank @= v

makePlayfield :: MonadAtari ()
makePlayfield = do
    pf0' <- load pf0
    pf1' <- load pf1
    pf2' <- load pf2
    ctrlpf' <- load ctrlpf
    let pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    pf @= pf'

{- INLINABLE readStella -}
readStella :: Word16 -> MonadAtari Word8
readStella addr = 
    case addr of
        0x00 -> load cxm0p
        0x01 -> load cxm1p
        0x02 -> load cxp0fb
        0x03 -> load cxp1fb
        0x04 -> load cxm0fb
        0x05 -> load cxm1fb
        0x06 -> load cxblpf
        0x07 -> load cxppmm
        0x0c -> load inpt4
        0x0d -> load inpt5
        0x0e -> liftIO $ do
                    putStrLn "Illegal read 0xe"
                    return 0xe
        0x0f -> liftIO $ do
                    putStrLn "Illegal read 0xf"
                    return 0xf -- Hack for Haunted House
        0x10 -> load cxm0p
        0x11 -> load cxm1p
        0x12 -> load cxp0fb
        0x13 -> load cxp1fb
        0x14 -> load cxm0fb
        0x15 -> load cxm1fb
        0x16 -> load cxblpf
        0x17 -> load cxppmm
        0x1c -> load inpt4
        0x1d -> load inpt5
        0x20 -> load cxm0p
        0x21 -> load cxm1p
        0x22 -> load cxp0fb
        0x23 -> load cxp1fb
        0x24 -> load cxm0fb
        0x25 -> load cxm1fb
        0x26 -> load cxblpf
        0x27 -> load cxppmm
        0x2c -> load inpt4
        0x2d -> load inpt5
        0x30 -> load cxm0p
        0x31 -> load cxm1p
        0x32 -> load cxp0fb
        0x33 -> load cxp1fb
        0x34 -> load cxm0fb
        0x35 -> load cxm1fb
        0x36 -> load cxblpf
        0x37 -> load cxppmm
        0x3c -> load inpt4
        0x3d -> load inpt5
        0x280 -> load swcha
        0x282 -> load swchb
        0x284 -> load intim
        0x285 -> load timint
        _ -> return 0 

{- INLINE stellaVsync -}
stellaVsync :: Word8 -> MonadAtari ()
stellaVsync v = do
    oldv <- load vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
        vpos @= 0
        renderDisplay
    vsync @= v

{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- load hpos
    -- Run instructions until we're at start of new scan line
    when (hpos' > 0) $ do
        stellaTickFor 1 -- there's a smarter way to do this XXX
        stellaWsync

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

stellaTickFor :: Int -> MonadAtari ()
stellaTickFor d = do
    n <- load ahead
    if d > n
        then do
            stellaTickFor' (d-n)
            ahead @= 0
        else ahead @= (n-d)

stellaTickFor' :: Int -> MonadAtari ()
stellaTickFor' diff = do
    when (diff >= 0) $ do
        -- Batch together items that don't need to be
        -- carried out on individual ticks
        modifyStellaClock id (+ fromIntegral diff)
        replicateM_ (fromIntegral diff) $ timerTick
        resmp0' <- load resmp0
        resmp1' <- load resmp1
        -- XXX surely this must be done every time - collisions
        clampMissiles resmp0' resmp1'

        ptr' <- view textureData
        -- XXX Not sure stellaDebug actually changes here so may be some redundancy
        stellaTick (fromIntegral diff) ptr'

{-# INLINE pureReadRom #-}
-- | pureReadRom sees address in full 6507 range 0x0000-0x1fff
pureReadRom :: Word16 -> MonadAtari Word8
pureReadRom addr = do
    atari <- ask
    let m = atari ^. rom
    let bankStateRef = atari ^. bankState
    bankState' <- liftIO $ readIORef bankStateRef
    let bankedAddress = bankAddress bankState' addr
    liftIO $ readArray m bankedAddress

{-# INLINE pureWriteRom #-}
-- | pureWriteRom sees address in full 6507 range 0x0000-0x1fff
-- You can write to Super Chip "ROM"
pureWriteRom :: Word16 -> Word8 -> MonadAtari ()
pureWriteRom addr v = do
    atari <- ask
    let m = atari ^. rom
    let bankStateRef = atari ^. bankState
    bankState' <- liftIO $ readIORef bankStateRef
    when (bankWritable bankState' addr) $ do
        let bankedAddress = bankAddress bankState' addr
        liftIO $ writeArray m bankedAddress v

{-# INLINE pureReadMemory #-}
-- | pureReadMemory expects an address in range 0x0000-0x1fff
-- The 'pure' refers to the fact that there are no side effects,
-- i.e. it won't trigger bank switching.
pureReadMemory :: MemoryType -> Word16 -> MonadAtari Word8
pureReadMemory ROM  addr = pureReadRom addr
pureReadMemory TIA  addr = readStella (addr .&. 0x3f)
pureReadMemory RIOT addr = readStella (0x280+(addr .&. 0x1f))
pureReadMemory RAM  addr = do
    atari <- ask
    let m = atari ^. ram
    liftIO $ readArray m (iz addr .&. 0x7f)

{-# INLINE pureWriteMemory #-}
pureWriteMemory :: MemoryType -> Word16 -> Word8 -> MonadAtari ()
pureWriteMemory TIA  addr v = writeStella (addr .&. 0x3f) v
pureWriteMemory RIOT addr v = writeStella (0x280+(addr .&. 0x1f)) v
pureWriteMemory ROM  addr v = pureWriteRom addr v
pureWriteMemory RAM  addr v = do
    atari <- ask
    let m = atari ^. ram
#if TRACE
    let r = atari ^. record
    i <- liftIO $ readIORef (atari ^. recordPtr)
#endif
    let realAddress = iz addr .&. 0x7f
    liftIO $ writeArray m realAddress v
#if TRACE
    liftIO $ writeArray r i (i8 realAddress)
    liftIO $ writeArray r (i+1) v
    liftIO $ writeIORef (atari ^. recordPtr) (i+2)
#endif

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr' = do
        let addr = addr' .&. 0x1fff -- 6507
        byte <- pureReadMemory (memoryType addr) addr

        atari <- ask
        let bankStateRef = atari ^. bankState
        liftIO $ modifyIORef bankStateRef $ bankSwitch addr 0

        return byte

    {-# INLINE writeMemory #-}
    writeMemory addr' v = do
        let addr = addr' .&. 0x1fff -- 6507
        pureWriteMemory (memoryType addr) addr v

        atari <- ask
        let bankStateRef = atari ^. bankState
        liftIO $ modifyIORef bankStateRef $ bankSwitch addr v

    {-# INLINE getPC #-}
    getPC = load pc
    {-# INLINE tick #-}
    tick n = do
        modifyClock id (+ fromIntegral n)
        -- c <- useClock id
        stellaTickFor (3*n)
    {-# INLINE putC #-}
    putC b = do { p' <- load p; p @= (p' & bitAt 0 .~ b) }
    {-# INLINE getC #-}
    getC = do { p' <- load p; return (p' ^. bitAt 0) }
    {-# INLINE putZ #-}
    putZ b = do { p' <- load p; p @= (p' & bitAt 1 .~ b) }
    {-# INLINE getZ #-}
    getZ = do { p' <- load p; return (p' ^. bitAt 1) }
    {-# INLINE putI #-}
    putI b = do { p' <- load p; p @= (p' & bitAt 2 .~ b) }
    {-# INLINE getI #-}
    getI = do { p' <- load p; return (p' ^. bitAt 2) }
    {-# INLINE putD #-}
    putD b = do { p' <- load p; p @= (p' & bitAt 3 .~ b) }
    {-# INLINE getD #-}
    getD = do { p' <- load p; return (p' ^. bitAt 3) }
    {-# INLINE putB #-}
    putB b = do { p' <- load p; p @= (p' & bitAt 4 .~ b) }
    {-# INLINE getB #-}
    getB = do { p' <- load p; return (p' ^. bitAt 4) }
    {-# INLINE putV #-}
    putV b = do { p' <- load p; p @= (p' & bitAt 6 .~ b) }
    {-# INLINE getV #-}
    getV = do { p' <- load p; return (p' ^. bitAt 6) }
    {-# INLINE putN #-}
    putN b = do { p' <- load p; p @= (p' & bitAt 7 .~ b) }
    {-# INLINE getN #-}
    getN = do { p' <- load p; return (p' ^. bitAt 7) }
    {-# INLINE getA #-}
    getA = load a
    {-# INLINE putA #-}
    putA r = a @= r
    {-# INLINE getS #-}
    getS = load s
    {-# INLINE putS #-}
    putS r = s @= r
    {-# INLINE getX #-}
    getX = load x
    {-# INLINE putX #-}
    putX r = x @= r 
    {-# INLINE getP #-}
    getP = load p
    {-# INLINE putP #-}
    putP r = p @= r 
    {-# INLINE getY #-}
    getY = load y
    {-# INLINE putY #-}
    putY r = y @= r
    {-# INLINE putPC #-}
    putPC r = pc @= r
    {-# INLINE addPC #-}
    addPC n = modify pc (+ fromIntegral n)

    {-# INLINE debugStr #-}
    debugStr _ _ = return ()
    {-# INLINE debugStrLn #-}
    debugStrLn _ _ = return ()

    {- INLINE illegal -}
    illegal i = do
        dumpState
        error $ "Illegal opcode 0x" ++ showHex i ""

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
    regPC <- getPC
    liftIO $ putStr $ " pc = " ++ showHex regPC ""
    regP <- getP
    liftIO $ do
        putStr $ " flags = " ++ showHex regP ""
        putStr $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
        putStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
        putStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
        putStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
        putStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
        putStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
        putStr $ ",C=" ++ showHex (regP .&. 1) ""
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

graphicsDelay :: Int -> MonadAtari ()
graphicsDelay d = do
    n <- load ahead
    when (d > n) $ do
            stellaTickFor' (d-n)
            ahead @= d

{- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = do
    case addr of
       0x00 -> stellaVsync v             -- VSYNC
       0x01 -> stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> graphicsDelay 4 >> nusiz0 @= v        -- NUSIZ0
       0x05 -> nusiz1 @= v        -- NUSIZ1
       0x06 -> (pcStep @-> pcColup0) >> colup0 @= v               -- COLUP0
       0x07 -> (pcStep @-> pcColup1) >> colup1 @= v               -- COLUP1
       0x08 -> (pcStep @-> pcColupf) >> colupf @= v               -- COLUPF
       0x09 -> (pcStep @-> pcColubk) >> colubk @= v               -- COLUBK
       0x0a -> ctrlpf @= v >> makePlayfield               -- CTRLPF
       0x0b -> graphicsDelay 0 >> refp0 @= v               -- REFP0
       0x0c -> graphicsDelay 0 >> refp1 @= v               -- REFP1
       -- I'm sure I read delay should be 3 for PF registers
       -- but that doesn't make sense to me.
       -- See docs/adventure_pf_timing.txt
       0x0d -> (pcStep @-> pcPf0) >> graphicsDelay 3 >> pf0 @= v >> makePlayfield    -- PF0
       0x0e -> (pcStep @-> pcPf1) >> graphicsDelay 3 >> pf1 @= v >> makePlayfield    -- PF1
       0x0f -> (pcStep @-> pcPf2) >> graphicsDelay 3 >> pf2 @= v >> makePlayfield    -- PF2
       0x10 -> (pcStep @-> pcResp0) >> graphicsDelay 5 >> hpos @-> ppos0 -- RESP0
       0x11 -> (pcStep @-> pcResp1) >> graphicsDelay 5 >> hpos @-> ppos1 -- RESP1
       0x12 -> (pcStep @-> pcResm0) >> graphicsDelay 4 >> hpos @-> mpos0 -- RESM0
       0x13 -> (pcStep @-> pcResm1) >> graphicsDelay 4 >> hpos @-> mpos1 -- RESM1
       0x14 -> (pcStep @-> pcResbl) >> graphicsDelay 4 >> load hpos >>= (return . max (picx+2)) >>= (bpos @=)  -- RESBL
       -- graphicsDelay of 1 chosen to stop spurious pixel in
       -- "CCE" in Freeway.
       0x1b -> do -- GRP0
                graphicsDelay 1
                newGrp0 @= v
                newGrp1 @-> oldGrp1
       0x1c -> do -- GRP1
                graphicsDelay 1
                newGrp1 @= v
                newGrp0 @-> oldGrp0
                newBall @-> oldBall
       0x1d -> enam0 @= v                -- ENAM0
       0x1e -> enam1 @= v                -- ENAM1
       0x1f -> newBall @= testBit v 1   -- ENABL
       0x20 -> hmp0 @= v                 -- HMP0
       0x21 -> hmp1 @= v                 -- HMP1
       0x22 -> hmm0 @= v                 -- HMM0
       0x23 -> hmm1 @= v                 -- HMM1
       0x24 -> hmbl @= v                 -- HMBL
       0x25 -> delayP0 @= testBit v 0   -- VDELP0
       0x26 -> delayP1 @= testBit v 0   -- VDELP1
       0x27 -> delayBall @= testBit v 0   -- VDELBL
       0x28 -> resmp0 @= v
       0x29 -> resmp1 @= v
       0x2a -> stellaHmove               -- HMOVE
       0x2b -> stellaHmclr               -- HMCLR
       0x2c -> stellaCxclr               -- CXCLR
       0x294 -> startIntervalTimerN 1 v
       0x295 -> startIntervalTimerN 8 v
       0x296 -> startIntervalTimerN 64 v
       0x297 -> startIntervalTimerN 1024 v
       _ -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

renderDisplay :: MonadAtari ()
renderDisplay = do
    window <- view sdlWindow
    prog <- view glProg
    attrib <- view glAttrib
    tex' <- view tex
    ptr <- view textureData
    windowWidth' <- view windowWidth
    windowHeight' <- view windowHeight
    liftIO $ updateTexture tex' ptr
    liftIO $ draw window windowWidth' windowHeight' prog attrib
    return ()

loopUntil :: Int64 -> MonadAtari ()
loopUntil n = do
    stellaClock' <- useStellaClock id
    when (stellaClock' < n) $ (pc @-> pcStep) >> step >> loopUntil n

initHardware :: MonadAtari ()
initHardware = do
    store inpt4 0x80
    store inpt5 0x80
    store swcha 0b11111111
    store swchb 0b00001011
    store xbreak (-1)
    store ybreak (-1)
    pclo <- readMemory 0x1ffc
    pchi <- readMemory 0x1ffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)
    liftIO $ putStrLn $ "Starting at address: 0x" ++ showHex initialPC ""
    store pc initialPC
