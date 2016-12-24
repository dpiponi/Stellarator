{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Emulation(stellaDebug,
                 dumpRegisters,
                 dumpState,
                 setBreak,
                 clock,
                 stellaClock,
                 initState,
                 trigger1,
                 load) where

--import Asm
import Atari2600
import BitManips
import Control.Lens
import Control.Monad.Reader
import Core
import Data.Array.IO
import Data.Bits hiding (bit)
import Data.Bits.Lens
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
import SDL.Vect
import SDL.Video
import Asm
import VideoOps
import qualified SDL

timerTick' :: Word8 -> Int -> Int -> (Word8, Int, Int)
timerTick' 0      0         _         = ((-1), (3*1-1), 1)
timerTick' intim' 0         interval' = ((intim'-1), (3*interval'-1), interval')
timerTick' intim' subtimer' interval' = (intim', (subtimer'-1), interval')

timerTick :: MonadAtari ()
timerTick = do
    (intim'', subtimer'', interval'') <- timerTick' <$> load intim <*> load subtimer <*> load interval
    store intim intim''
    store subtimer subtimer''
    store interval interval''

startIntervalTimerN :: Int -> Word8 -> MonadAtari ()
startIntervalTimerN n v = do
    store interval n
    store subtimer (3*n-1)
    store intim v

initState :: IOUArray Int Word8 ->
             BankMode ->
             IOUArray Int Word8 ->
             Word16 ->
             SDL.Surface -> SDL.Surface ->
             SDL.Window -> IO Atari2600
initState ram' mode rom' initialPC
          helloWorld screenSurface window = do
          stellaDebug' <- newIORef DebugState.start
          memory' <- newIORef $ Memory { _bankMode = mode }
          clock' <- newIORef 0
          -- debug' <- newIORef 8
          stellaClock' <- newIORef 0
          boolArray' <- newArray (0, maxBool) False
          intArray' <- newArray (0, 127) 0      -- Overkill
          word64Array' <- newArray (0, maxWord64) 0
          word16Array' <- newArray (0, 127) 0      -- Overkill
          word8Array' <- newArray (0, maxWord8) 0
          liftIO $ st word16Array' pc initialPC
          return $ Atari2600 {
              _rom = rom',
              _ram = ram',
              _stellaDebug = stellaDebug',
              _memory = memory',
              _clock = clock',
              _stellaClock = stellaClock',
              _sdlBackSurface = helloWorld,
              _sdlFrontSurface = screenSurface,
              _sdlFrontWindow = window,
              _boolArray = boolArray',
              _intArray = intArray',
              _word64Array = word64Array',
              _word16Array = word16Array',
              _word8Array = word8Array'
          }

{- INLINE stellaHmclr -}
stellaHmclr :: MonadAtari ()
stellaHmclr =
    mapM_ (flip store 0) [hmp0, hmp1, hmm0, hmm1, hmbl]

{- INLINE stellaCxclr -}
stellaCxclr :: MonadAtari ()
stellaCxclr =
    mapM_ (flip store 0) [cxm0p, cxm1p, cxm0fb, cxm1fb, cxp0fb, cxp1fb, cxblpf, cxppmm]

{- INLINE stellaHmove -}
stellaHmove :: MonadAtari ()
stellaHmove = do
    poffset0 <- load hmp0
    modify s_ppos0 $ \ppos0' ->  wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- load hmp1
    modify s_ppos1 $ \ppos1' ->  wrap160 (ppos1'-clockMove poffset1)

    moffset0 <- load hmm0
    modify s_mpos0 $ \mpos0' ->  wrap160 (mpos0'-clockMove moffset0)

    moffset1 <- load hmm1
    modify s_mpos1 $ \mpos1' ->  wrap160 (mpos1'-clockMove moffset1)

    boffset <- load hmbl
    bpos' <- load s_bpos
    modify s_bpos $ \bpos' -> wrap160 (bpos'-clockMove boffset)

{-
-- Are these needed?
{- INLINE stellaResmp0 -}
stellaResmp0 :: MonadAtari ()
stellaResmp0 = do
    playerPosition <- load s_ppos0
    store s_mpos0 (playerPosition :: Int)

{- INLINE stellaResmp1 -}
stellaResmp1 :: MonadAtari ()
stellaResmp1 = do
    playerPosition <- load s_ppos1
    store s_mpos1 (playerPosition :: Int)
-}

{- INLINE stellaDebugStr -}
stellaDebugStr :: Int -> String -> MonadAtari ()
stellaDebugStr n str = do
    d <- useStellaDebug debugLevel
    if n <= d
        then do
            liftIO $ putStr str
        else return ()

{- INLINE stellaDebugStrLn -}
stellaDebugStrLn :: Int -> String -> MonadAtari ()
stellaDebugStrLn n str = do
    d <- useStellaDebug debugLevel
    if n <= d
        then do
            liftIO $ putStrLn str
        else return ()

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
    store vblank v

makePlayfield :: MonadAtari ()
makePlayfield = do
    pf0' <- load pf0
    pf1' <- load pf1
    pf2' <- load pf2
    ctrlpf' <- load ctrlpf
    let pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    store pf pf'

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
        _ -> return 0 

{- INLINE stellaVsync -}
stellaVsync :: Word8 -> MonadAtari ()
stellaVsync v = do
    oldv <- load vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
        store hpos 0
        store vpos 0
    store vsync v
    renderDisplay

{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- load hpos
    -- Run instructions until we're at start of new scan line
    when (hpos' > 0) $ do
        --modifyClock id (+ 1)
        --clock' <- useClock id
        stellaTickFor 1 -- there's a smarter way to do this XXX
        stellaWsync

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

stellaTickFor :: Int -> MonadAtari ()
stellaTickFor d = do
    n <- load ahead
    if d > n
        then do
            stellaTickFor' (d-n)
            store ahead 0
        else store ahead (n-d)

stellaTickFor' :: Int -> MonadAtari ()
stellaTickFor' diff = do
    --c <- useStellaClock id
    --let diff = n-c
    when (diff >= 0) $ do
        -- Batch together items that don't need to be
        -- carried out on individual ticks
        modifyStellaClock id (+ fromIntegral diff)
        replicateM_ (fromIntegral diff) $ timerTick
        resmp0' <- load resmp0
        resmp1' <- load resmp1
        -- XXX surely this must be done every time - collisions
        clampMissiles resmp0' resmp1'

        stellaDebug' <- useStellaDebug id
        surface <- getBackSurface
        ptr <- liftIO $ surfacePixels surface -- <-- XXX I think it's OK but not sure
        let ptr' = castPtr ptr :: Ptr Word32
        -- XXX Not sure stellaDebug actually changes here so may be some redundancy
        stellaTick (fromIntegral diff) ptr'

{-# INLINE pureReadRom #-}
pureReadRom :: Word16 -> MonadAtari Word8
pureReadRom addr = do
    atari <- ask
    let m = atari ^. rom
    offset <- load bankOffset
    byte <- liftIO $ readArray m ((iz addr .&. 0xfff)+fromIntegral offset)
    return byte

{-# INLINE bankSwitch #-}
bankSwitch :: BankMode -> Word16 -> Word16 -> Word16
bankSwitch _        addr  old | addr < 0x1ff6 = old
bankSwitch UnBanked _      _    = 0
bankSwitch F8       0x1ff8 _    = 0
bankSwitch F8       0x1ff9 _    = 0x1000
bankSwitch F6       0x1ff6 _    = 0
bankSwitch F6       0x1ff7 _    = 0x1000
bankSwitch F6       0x1ff8 _    = 0x2000
bankSwitch F6       0x1ff9 _    = 0x3000
bankSwitch _        _      old = old

{-# INLINE pureReadMemory #-}
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
pureWriteMemory ROM  _    _ = return ()
pureWriteMemory RAM  addr v = do
    atari <- ask
    let m = atari ^. ram
    liftIO $ writeArray m (iz addr .&. 0x7f) v

instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr' = do
        let addr = addr' .&. 0x1fff -- 6507
        byte <- pureReadMemory (memoryType addr) addr
        bankType <- useMemory bankMode
        modify bankOffset $ bankSwitch bankType addr
        return byte

    {-# INLINE writeMemory #-}
    writeMemory addr' v = do
        let addr = addr' .&. 0x1fff -- 6507
        pureWriteMemory (memoryType addr) addr v
        bankType <- useMemory bankMode
        modify bankOffset $ bankSwitch bankType addr

    {-# INLINE getPC #-}
    getPC = load pc
    {-# INLINE tick #-}
    tick n = do
        modifyClock id (+ fromIntegral n)
        -- c <- useClock id
        stellaTickFor (3*n)
    {-# INLINE putC #-}
    putC b = do { p' <- load p; store p (p' & bitAt 0 .~ b) }
    {-# INLINE getC #-}
    getC = do { p' <- load p; return (p' ^. bitAt 0) }
    {-# INLINE putZ #-}
    putZ b = do { p' <- load p; store p (p' & bitAt 1 .~ b) }
    {-# INLINE getZ #-}
    getZ = do { p' <- load p; return (p' ^. bitAt 1) }
    {-# INLINE putI #-}
    putI b = do { p' <- load p; store p (p' & bitAt 2 .~ b) }
    {-# INLINE getI #-}
    getI = do { p' <- load p; return (p' ^. bitAt 2) }
    {-# INLINE putD #-}
    putD b = do { p' <- load p; store p (p' & bitAt 3 .~ b) }
    {-# INLINE getD #-}
    getD = do { p' <- load p; return (p' ^. bitAt 3) }
    {-# INLINE putB #-}
    putB b = do { p' <- load p; store p (p' & bitAt 4 .~ b) }
    {-# INLINE getB #-}
    getB = do { p' <- load p; return (p' ^. bitAt 4) }
    {-# INLINE putV #-}
    putV b = do { p' <- load p; store p (p' & bitAt 6 .~ b) }
    {-# INLINE getV #-}
    getV = do { p' <- load p; return (p' ^. bitAt 6) }
    {-# INLINE putN #-}
    putN b = do { p' <- load p; store p (p' & bitAt 7 .~ b) }
    {-# INLINE getN #-}
    getN = do { p' <- load p; return (p' ^. bitAt 7) }
    {-# INLINE getA #-}
    getA = load a
    {-# INLINE putA #-}
    putA r = store a r
    {-# INLINE getS #-}
    getS = load s
    {-# INLINE putS #-}
    putS r = store s r
    {-# INLINE getX #-}
    getX = load x
    {-# INLINE putX #-}
    putX r = store x r 
    {-# INLINE getP #-}
    getP = load p
    {-# INLINE putP #-}
    putP r = store p r 
    {-# INLINE getY #-}
    getY = load y
    {-# INLINE putY #-}
    putY r = store y r
    {-# INLINE putPC #-}
    putPC r = store pc r
    {-# INLINE addPC #-}
    addPC n = modify pc (+ fromIntegral n)

    {-# INLINE debugStr #-}
    debugStr _ _ = return ()
    {-# INLINE debugStrLn #-}
    debugStrLn _ _ = return ()

    {- INLINE illegal -}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

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

{- INLINE setBreak -}
setBreak :: Int -> Int -> MonadAtari ()
setBreak breakX breakY = do
    store xbreak (breakX+picx)
    store ybreak (breakY+picy)

graphicsDelay :: Int -> MonadAtari ()
graphicsDelay d = do
    n <- load ahead
    when (d > n) $ do
            stellaTickFor' (d-n)
            store ahead d

{- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = do
    case addr of
       0x00 -> stellaVsync v             -- VSYNC
       0x01 -> stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> store nusiz0 v        -- NUSIZ0
       0x05 -> store nusiz1 v        -- NUSIZ1
       0x06 -> store colup0 v               -- COLUP0
       0x07 -> store colup1 v               -- COLUP1
       0x08 -> store colupf v               -- COLUPF
       0x09 -> store colubk v               -- COLUBK
       0x0a -> store ctrlpf v >> makePlayfield               -- COLUPF
       0x0b -> store refp0 v               -- REFP0
       0x0c -> store refp1 v               -- REFP1
       -- I'm sure I read delay should be 3 for PF registers
       -- but that doesn't make sense to me.
       -- See docs/adventure_pf_timing.txt
       0x0d -> graphicsDelay 3 >> store pf0 v >> makePlayfield                  -- PF0
       0x0e -> graphicsDelay 3 >> store pf1 v >> makePlayfield                  -- PF1
       0x0f -> graphicsDelay 3 >> store pf2 v >> makePlayfield                  -- PF2
       0x10 -> graphicsDelay 5 >> load hpos >>= store s_ppos0 -- RESP0
       0x11 -> graphicsDelay 5 >> load hpos >>= store s_ppos1 -- RESP1
       0x12 -> graphicsDelay 4 >> load hpos >>= store s_mpos0 -- RESM0
       0x13 -> graphicsDelay 4 >> load hpos >>= store s_mpos1 -- RESM1
       0x14 -> graphicsDelay 4 >> load hpos >>= (return . max (picx+2)) >>= store s_bpos  -- RESBL
       0x1b -> do -- GRP0
                store newGrp0 v
                load newGrp1 >>= store oldGrp1
       0x1c -> do -- GRP1
                store newGrp1 v
                load newGrp0 >>= store oldGrp0
                load newBall >>= store oldBall
       0x1d -> store enam0 v                -- ENAM0
       0x1e -> store enam1 v                -- ENAM1
       0x1f -> store newBall $ testBit v 1   -- ENABL
       0x20 -> store hmp0 v                 -- HMP0
       0x21 -> store hmp1 v                 -- HMP1
       0x22 -> store hmm0 v                 -- HMM0
       0x23 -> store hmm1 v                 -- HMM1
       0x24 -> store hmbl v                 -- HMBL
       0x25 -> store delayP0 $ testBit v 0   -- VDELP0
       0x26 -> store delayP1 $ testBit v 0   -- VDELP1
       0x27 -> store delayBall $ testBit v 0   -- VDELBL
       0x28 -> store resmp0 v
       0x29 -> store resmp1 v
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
    back <- getBackSurface
    front <- getFrontSurface
    window <- getFrontWindow
    unlockSurface back
    surfaceBlitScaled back Nothing front
                (Just (Rectangle (P (V2 0 0))
                                 (V2 (fromIntegral $ screenWidth*xscale) (fromIntegral $ screenHeight*yscale))))
    lockSurface back
    updateWindowSurface window
