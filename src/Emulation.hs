{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Emulation(stellaDebug,
                 dumpStella,
                 dumpRegisters,
                 dumpState,
                 setBreak,
                 clock,
                 stellaClock,
                 initState,
                 getIRegister,
                 putIRegister,
                 trigger1,
                 modifyIRegister,
                 getORegister) where

import Atari2600
import BitManips
import Control.Lens
import Control.Monad.Reader
import Core
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.Data
import Data.IORef
import Data.Int
import Data.Word
import DebugState
import Disasm
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Memory
import Metrics
import Numeric
import Prelude hiding (last)
import SDL.Vect
import SDL.Video
import SDL.Video.Renderer
import Stella.TIARegisters
import TIAColors
import VideoOps
import qualified SDL

timerTick' :: Word8 -> Int -> Int -> (Word8, Int, Int)
timerTick' 0      0         interval' = ((-1), (3*1-1), 1)
timerTick' intim' 0         interval' = ((intim'-1), (3*interval'-1), interval')
timerTick' intim' subtimer' interval' = (intim', (subtimer'-1), interval')

timerTick :: Segment Int -> Segment Word8 -> IO ()
timerTick intr word8r = do
    intim' <- ld word8r intim
    subtimer' <- ld intr subtimer
    interval' <- ld intr interval
    let (intim'', subtimer'', interval'') = timerTick' intim' subtimer' interval'
    st word8r intim intim''
    st intr subtimer subtimer''
    st intr interval interval''

startIntervalTimer :: Segment Int -> Segment Word8 -> IO ()
startIntervalTimer intr word8r = do
    st word8r intim 0
    st intr subtimer 0
    st intr interval 0

startIntervalTimerN :: Segment Int -> Segment Word8 -> Int -> Word8 -> IO ()
startIntervalTimerN intr word8r n v = do
    st intr interval n
    st intr subtimer (3*n-1)
    st word8r intim v

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
          memory' <- newIORef $ Memory {
                  _bankMode = mode,
                  _bankOffset = 0
              }
          --sprites' <- newIORef Stella.Sprites.start
          stellaDebug' <- newIORef DebugState.start
          clock' <- newIORef 0
          debug' <- newIORef 8
          --graphics' <- newIORef Stella.Graphics.start
          stellaClock' <- newIORef 0
          boolArray' <- newArray (0, 127) False -- Overkill
          intArray' <- newArray (0, 127) 0      -- Overkill
          word64Array' <- newArray (0, 127) 0      -- Overkill
          word16Array' <- newArray (0, 127) 0      -- Overkill
          word8Array' <- newArray (0, 127) 0      -- Overkill
          liftIO $ st word16Array' pc initialPC
          startIntervalTimer intArray' word8Array'
          return $ Atari2600 {
              _rom = rom',
              _ram = ram',
              _stellaDebug = stellaDebug',
              _memory = memory',
              -- _regs = regs',
              _clock = clock',
              _debug = debug',
              --_sprites = sprites',
              --_intervalTimer = intervalTimer',
              --_graphics = graphics',
              _stellaClock = stellaClock',
              _oregisters = oregs,
              _iregisters = iregs,
              _sdlBackSurface = helloWorld,
              _sdlFrontSurface = screenSurface,
              _sdlFrontWindow = window,
              _boolArray = boolArray',
              _intArray = intArray',
              _word64Array = word64Array',
              _word16Array = word16Array',
              _word8Array = word8Array'
          }

{-# INLINE putORegister #-}
putORegister :: OReg -> Word8 -> MonadAtari ()
putORegister i v = do
    r <- getORegisters
    liftIO $ writeArray r i v

{-# INLINE getORegister #-}
getORegister :: OReg -> MonadAtari Word8
getORegister i = do
    r <- getORegisters
    liftIO $ readArray r i

{-# INLINE putIRegister #-}
putIRegister :: IReg -> Word8 -> MonadAtari ()
putIRegister i v = do
    r <- getIRegisters
    liftIO $ writeArray r i v

{-# INLINE modifyIRegister #-}
modifyIRegister :: IReg -> (Word8 -> Word8) -> MonadAtari ()
modifyIRegister i f = do
    r <- getIRegisters
    liftIO $ (readArray r i >>= writeArray r i . f)

{-# INLINE getIRegister #-}
getIRegister :: IReg -> MonadAtari Word8
getIRegister i = do
    r <- getIRegisters
    liftIO $ readArray r i

{- INLINE stellaHmclr -}
stellaHmclr :: MonadAtari ()
stellaHmclr = do
    r <- getORegisters
    liftIO $ mapM_ (flip (fastPutORegister r) 0) [hmp0, hmp1,
                                                  hmm0, hmm1, hmbl]

{- INLINE stellaCxclr -}
stellaCxclr :: MonadAtari ()
stellaCxclr = do
    r <- getIRegisters
    liftIO $ mapM_ (flip (fastPutIRegister r) 0) [cxm0p, cxm1p, cxm0fb,
                                                  cxm1fb, cxp0fb, cxp1fb,
                                                  cxblpf, cxppmm]

{- INLINE stellaHmove -}
stellaHmove :: MonadAtari ()
stellaHmove = do
    r <- getORegisters
    let getOReg reg = liftIO $ fastGetORegister r reg

    poffset0 <- getOReg hmp0
    modify s_ppos0 $ \ppos0' ->  wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- getOReg hmp1
    modify s_ppos1 $ \ppos1' ->  wrap160 (ppos1'-clockMove poffset1)

    moffset0 <- getOReg hmm0
    modify s_mpos0 $ \mpos0' ->  wrap160 (mpos0'-clockMove moffset0)

    moffset1 <- getOReg hmm1
    modify s_mpos1 $ \mpos1' ->  wrap160 (mpos1'-clockMove moffset1)

    boffset <- getOReg hmbl
    modify s_bpos $ \bpos' -> wrap160 (bpos'-clockMove boffset)

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

{-# INLINE clockMove #-}
clockMove :: Word8 -> Int
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

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
    ir <- getIRegisters
    or <- getORegisters
    boolr <- getBoolArray
    trigger1' <- load trigger1
    if not trigger1'
        then do
            i <- liftIO $ fastGetIRegister ir inpt4 -- XXX write modifyIRegister
            liftIO $ fastPutIRegister ir inpt4 (setBit i 7)
        else do
            i <- liftIO $ fastGetIRegister ir inpt4 -- XXX write modifyIRegister
            liftIO $ fastPutIRegister ir inpt4 (clearBit i 7)

    liftIO $ fastPutORegister or vblank v

makePlayfield :: MonadAtari ()
makePlayfield = do
    r <- getORegisters
    pf0' <- liftIO $ fastGetORegister r pf0
    pf1' <- liftIO $ fastGetORegister r pf1
    pf2' <- liftIO $ fastGetORegister r pf2
    ctrlpf' <- liftIO $ fastGetORegister r ctrlpf
    let pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    word64r <- getWord64Array
    liftIO $ st word64r pf pf'

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
        0x284 -> load intim {-do
            word8r <- view word8Array
            liftIO $ ld word8r intim-}
        _ -> return 0 -- (liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr "") >> return 0

{- INLINE stellaVsync -}
stellaVsync :: Word8 -> MonadAtari ()
stellaVsync v = do
    or <- getORegisters
    oldv <- liftIO $ fastGetORegister or vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
        intr <- view intArray
        liftIO $ st intr hpos 0
        liftIO $ st intr vpos 0
    liftIO $ fastPutORegister or vsync v
    -- sdlState <- useHardware stellaSDL
    renderDisplay

{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    intr <- view intArray
    hpos' <- load hpos
    when (hpos' > 2) $ do
        modifyClock id (+ 1)
        clock' <- useClock id
        stellaTickUntil (3*clock')
        stellaWsync

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

stellaTickUntil :: Int64 -> MonadAtari ()
stellaTickUntil n = do
    c <- useStellaClock id
    let diff = n-c
    when (diff >= 0) $ do
        -- Batch together items that don't need to be
        -- carried out on individual ticks
        modifyStellaClock id (+ diff)
        r <- getORegisters
        ir <- getIRegisters
        intr <- view intArray
        word64r <- getWord64Array
        boolr <- getBoolArray
        word8r <- view word8Array
        -- it <- useIntervalTimer id
        -- putIntervalTimer id (church diff timerTick it)
        liftIO $ replicateM_ (fromIntegral diff) $ timerTick intr word8r
        resmp0' <- liftIO $ fastGetORegister r resmp0
        resmp1' <- liftIO $ fastGetORegister r resmp1
        clampMissiles resmp0' resmp1'

        stellaDebug' <- useStellaDebug id
        -- graphics' <- useGraphics id
        surface <- getBackSurface
        ptr <- liftIO $ surfacePixels surface -- <-- XXX I think it's OK but not sure
        let ptr' = castPtr ptr :: Ptr Word32
        -- sprites' <- useSprites id
        -- XXX Not sure stellaDebug actually changes here so may be some redundancy
        stellaDebug'' <- liftIO $ stellaTick (fromIntegral diff) word8r word64r intr boolr ir r stellaDebug' ptr'
        putStellaDebug id stellaDebug'' -- XX Does this update sprites??? XXX

{-# INLINE pureReadRom #-}
pureReadRom :: Word16 -> MonadAtari Word8
pureReadRom addr = do
    atari <- ask
    let m = atari ^. rom
    offset <- useMemory bankOffset
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
        modifyMemory bankOffset $ bankSwitch bankType addr
        return byte

    {-# INLINE writeMemory #-}
    writeMemory addr' v = do
        let addr = addr' .&. 0x1fff -- 6507
        pureWriteMemory (memoryType addr) addr v
        bankType <- useMemory bankMode
        modifyMemory bankOffset $ bankSwitch bankType addr

    {-# INLINE getPC #-}
    getPC = load pc
    {-# INLINE tick #-}
    tick n = do
        modifyClock id (+ fromIntegral n)
        c <- useClock id
        stellaTickUntil (3*c)
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

{-
    {- INLINE debugStr 9 -}
    debugStr n str = do
        d <- view debug
        if n <= d
            then liftIO $ putStr str
            else return ()

    {- INLINE debugStrLn 9 -}
    debugStrLn n str = do
        d <- view debug
        if n <= d
            then liftIO $ putStrLn str
            else return ()
-}

    {- INLINE illegal -}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""

dumpStella :: MonadAtari ()
dumpStella = do
    liftIO $ putStrLn "--------"
    hpos' <- load hpos
    vpos' <- load vpos
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    --graphics' <- view graphics
    --graphics'' <- liftIO $ readIORef graphics'
    grp0' <- load oldGrp0
    grp1' <- load oldGrp1
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
    enablOld <- load oldBall
    enablNew <- load newBall
    liftIO $ putStr $ "ENAM0 = " ++ show (testBit enam0' 1)
    liftIO $ putStr $ " ENAM1 = " ++ show (testBit enam1' 1)
    liftIO $ putStrLn $ " ENABL = " ++ show (enablOld, enablNew)
    -- sprites' <- view sprites
    -- sprites'' <- liftIO $ readIORef sprites'
    --let mpos0' = sprites'' ^. s_mpos0
    --let mpos1' = sprites'' ^. s_mpos1
    mpos0' <- load s_mpos0
    mpos1' <- load s_mpos1
    hmm0' <- getORegister hmm0
    hmm1' <- getORegister hmm1
    liftIO $ putStr $ "missile0 @ " ++ show mpos0' ++ "(" ++ show (clockMove hmm0') ++ ")"
    liftIO $ putStrLn $ " missile1 @ " ++ show mpos1' ++ "(" ++ show (clockMove hmm1') ++ ")"
    vdelp0' <- load delayP0
    vdelp1' <- load delayP1
    vdelbl' <- load delayBall
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
    --tClock <- view clock
    --putStr 9 $ "clock = " ++ show tClock
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
setBreak breakX breakY = putStellaDebug posbreak (breakX+picx, breakY+picy)

graphicsDelay :: Int64 -> MonadAtari ()
graphicsDelay n = do
    c <- useClock id
    stellaTickUntil (3*c+n)

{- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = do
    intr <- view intArray
    boolr <- view boolArray
    word8r <- view word8Array
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
       0x0a -> putORegister ctrlpf v >> makePlayfield               -- COLUPF
       0x0b -> putORegister refp0 v               -- REFP0
       0x0c -> putORegister refp1 v               -- REFP1
       0x0d -> graphicsDelay 4 >> putORegister pf0 v >> makePlayfield                  -- PF0
       0x0e -> graphicsDelay 4 >> putORegister pf1 v >> makePlayfield                  -- PF1
       0x0f -> graphicsDelay 4 >> putORegister pf2 v >> makePlayfield                  -- PF2
       0x10 -> graphicsDelay 5 >> load hpos >>= store s_ppos0 -- RESP0
       0x11 -> graphicsDelay 5 >> load hpos >>= store s_ppos1 -- RESP1
       0x12 -> graphicsDelay 4 >> load hpos >>= store s_mpos0 -- RESM0
       0x13 -> graphicsDelay 4 >> load hpos >>= store s_mpos1 -- RESM1
       0x14 -> graphicsDelay 4 >> load hpos >>= store s_bpos  -- RESBL
       0x1b -> do -- GRP0
                store newGrp0 v
                load newGrp1 >>= store oldGrp1
       0x1c -> do -- GRP1
                store newGrp1 v
                load newGrp0 >>= store oldGrp0
                load newBall >>= store oldBall
       0x1d -> putORegister enam0 v                -- ENAM0
       0x1e -> putORegister enam1 v                -- ENAM1
       0x1f -> liftIO $ st boolr newBall $ testBit v 1   -- ENABL
       0x20 -> putORegister hmp0 v                 -- HMP0
       0x21 -> putORegister hmp1 v                 -- HMP1
       0x22 -> putORegister hmm0 v                 -- HMM0
       0x23 -> putORegister hmm1 v                 -- HMM1
       0x24 -> putORegister hmbl v                 -- HMBL
       0x25 -> store delayP0 $ testBit v 0   -- VDELP0
       0x26 -> store delayP1 $ testBit v 0   -- VDELP1
       0x27 -> store delayBall $ testBit v 0   -- VDELBL
       0x28 -> putORegister resmp0 v
       0x29 -> putORegister resmp1 v
       0x2a -> stellaHmove               -- HMOVE
       0x2b -> stellaHmclr               -- HMCLR
       0x2c -> stellaCxclr               -- CXCLR
       0x294 -> liftIO $ startIntervalTimerN intr word8r 1 v
       0x295 -> liftIO $ startIntervalTimerN intr word8r 8 v
       0x296 -> liftIO $ startIntervalTimerN intr word8r 64 v
       0x297 -> liftIO $ startIntervalTimerN intr word8r 1024 v
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
