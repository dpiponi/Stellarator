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
import Control.Monad.State
import Core
import Data.Data
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.Int
import Data.Word
import DebugState
import Disasm
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import MemoryMap
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

data BankMode = UnBanked | F8 deriving (Show, Data, Typeable)

data Atari2600 = Atari2600 {
    _mem :: IOUArray Int Word8,
    _regs :: !Registers,
    _clock :: !Int64,
    _debug :: !Int,
    _oregisters :: IOUArray OReg Word8,
    _iregisters :: IOUArray IReg Word8,
    _bankMode :: BankMode,
    _bankOffset :: Word16,

    _stellaDebug :: DebugState,
    _stellaSDL :: SDLState,

    _position :: (CInt, CInt),
    _stellaClock :: !Int64,
    _graphics :: Graphics,
    _sprites :: Sprites,
    _intervalTimer :: IntervalTimer
}

$(makeLenses ''Atari2600)
$(makeLenses ''Registers)

newtype MonadAtari a = M { unM :: StateT Atari2600 IO a }
    deriving (Functor, Applicative, Monad, MonadState Atari2600, MonadIO)

initState :: BankMode ->
             IOUArray Int Word8 ->
             IOUArray OReg Word8 ->
             IOUArray IReg Word8 ->
             Word16 ->
             SDL.Surface -> SDL.Surface ->
             SDL.Window -> Atari2600
initState mode memory oregs iregs initialPC helloWorld screenSurface window = Atari2600 {
      _mem = memory,  _clock = 0, _regs = R initialPC 0 0 0 0 0xff,
      _debug = 8,

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
      _bankMode = mode,
      _bankOffset = 0
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

{-
instance Emu6502 MonadAtari where
    {-# INLINE readMemory #-}
    readMemory addr' =
        let addr = addr' .&. 0b1111111111111 in -- 6507
        if addr >= 0x1000
            then do
                m <- use mem
                offset <- use bankOffset
                byte <- liftIO $ readArray m (iz (addr-0x1000+offset))

                when (addr >= 0x1ff8) $ do
                    bankType <- use bankMode
                    case bankType of
                        UnBanked -> return ()
                        F8 -> do
                            when (addr == 0x1ff8) $ bankOffset .= 0
                            when (addr == 0x1ff9) $ bankOffset .= 0x1000

                return byte
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
                when (addr >= 0x1ff8) $ do
                    bankType <- use bankMode
                    case bankType of
                        UnBanked -> return ()
                        F8 -> do
                            when (addr == 0x1ff8) $ bankOffset .= 0
                            when (addr == 0x1ff9) $ bankOffset .= 0x1000
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
        clock += fromIntegral n
        c <- use clock
        stellaTickUntil (3*c)
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

{- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = 
    --(liftIO $ print $ "Hello!!!! " ++ showHex addr "" ++ " " ++ showHex v "") >>
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
       0x0d -> graphicsDelay 4 >> putORegister pf0 v                  -- PF0
       0x0e -> graphicsDelay 4 >> putORegister pf1 v                  -- PF1
       0x0f -> graphicsDelay 4 >> putORegister pf2 v                  -- PF2
       0x10 -> graphicsDelay 5 >> use hpos >>= (ppos0 .=)   -- RESP0 XXX FUDGE FACTORS
       0x11 -> graphicsDelay 5 >> use hpos >>= (ppos1 .=)   -- RESP1
       0x12 -> graphicsDelay 4 >> use hpos >>= (mpos0 .=)   -- RESM0
       0x13 -> graphicsDelay 4 >> use hpos >>= (mpos1 .=)   -- RESM1
       0x14 -> graphicsDelay 4 >> use hpos >>= (bpos .=)     -- RESBL
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

       0x294 -> intervalTimer .= start1 v -- TIM1T
       0x295 -> intervalTimer .= start8 v -- TIM8T
       0x296 -> intervalTimer .= start64 v -- TIM64T
       0x297 -> intervalTimer .= start1024 v -- TIM1024T
       _ -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

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
        _ -> return 0 -- (liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr "") >> return 0
-}

{-# INLINE putORegister #-}
putORegister :: OReg -> Word8 -> MonadAtari ()
putORegister i v = do
    r <- use oregisters
    liftIO $ writeArray r i v

{-# INLINE getORegister #-}
getORegister :: OReg -> MonadAtari Word8
getORegister i = do
    r <- use oregisters
    liftIO $ readArray r i

{-# INLINE putIRegister #-}
putIRegister :: IReg -> Word8 -> MonadAtari ()
putIRegister i v = do
    r <- use iregisters
    liftIO $ writeArray r i v

{-# INLINE modifyIRegister #-}
modifyIRegister :: IReg -> (Word8 -> Word8) -> MonadAtari ()
modifyIRegister i f = do
    r <- use iregisters
    liftIO $ (readArray r i >>= writeArray r i . f)

{-# INLINE getIRegister #-}
getIRegister :: IReg -> MonadAtari Word8
getIRegister i = do
    r <- use iregisters
    liftIO $ readArray r i

{-# INLINE orIRegister #-}
orIRegister :: IReg -> Word8 -> MonadAtari ()
orIRegister i v = modifyIRegister i (v .|.)

{- INLINE stellaHmclr -}
stellaHmclr :: MonadAtari ()
stellaHmclr = do
    putORegister hmp0 0
    putORegister hmp1 0
    putORegister hmm0 0
    putORegister hmm1 0
    putORegister hmbl 0

{- INLINE stellaCxclr -}
stellaCxclr :: MonadAtari ()
stellaCxclr = do
    putIRegister cxm0p 0
    putIRegister cxm1p 0
    putIRegister cxm0fb 0
    putIRegister cxm1fb 0
    putIRegister cxp0fb 0
    putIRegister cxp1fb 0
    putIRegister cxblpf 0
    putIRegister cxppmm 0

picy :: CInt
picy = 40
picx :: CInt
picx = 68

{- INLINE stellaHmove -}
stellaHmove :: MonadAtari ()
stellaHmove = do
    Sprites ppos0' ppos1' mpos0' mpos1' bpos' <- use sprites

    poffset0 <- getORegister hmp0
    let ppos0'' = wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- getORegister hmp1
    let ppos1'' = wrap160 (ppos1'-clockMove poffset1)

    moffset0 <- getORegister hmm0
    let mpos0'' = wrap160 (mpos0'-clockMove moffset0) -- XXX do rest

    moffset1 <- getORegister hmm1
    let mpos1'' = wrap160 (mpos1'-clockMove moffset1) -- XXX do rest

    boffset <- getORegister hmbl
    let bpos'' = wrap160 (bpos'-clockMove boffset)

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

{-
{- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- use hpos
    --stellaTick (233-fromIntegral hpos') -- 228
    --stellaTick (232-fromIntegral hpos') -- 228
    -- This isn't quite right. I think CPU clock should be able to shift
    -- phase relative to pixel click. XXX
    when (hpos' > 2) $ do
        clock += 1 -- sleep the CPU
        clock' <- use clock
        stellaTickUntil (3*clock')
        stellaWsync

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/


{- INLINE stellaVsync -}
stellaVsync :: Word8 -> MonadAtari ()
stellaVsync v = do
    oldv <- getORegister vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
            hpos .= 0
            vpos .= 0
    putORegister vsync v
    s <- use stellaSDL
    liftIO $ renderDisplay s

stellaTick :: Int -> MonadAtari ()
stellaTick n | n <= 0 = return ()
stellaTick n = do
    stella <- get
    let (xbreak', ybreak') = stella ^. stellaDebug . posbreak
    let (hpos', vpos') = stella ^. position
    when ((hpos', vpos') == (xbreak', ybreak')) $ do
        dumpStella
        stellaDebug . posbreak .= (-1, -1) -- Maybe maybe

    stellaClock += 1
    intervalTimer %= timerTick
    
    -- Display
    when (vpos' >= picy && vpos' < picy+192 && hpos' >= picx) $ do
        let !surface = _sdlBackSurface (_stellaSDL stella)
        !ptr <- liftIO $ surfacePixels surface
        let !ptr' = castPtr ptr :: Ptr Word32
        let !pixelx = hpos'-picx
        let !pixely = vpos'-picy
        let !i = screenWidth*pixely+pixelx

        stella <- get

        let r = _oregisters stella
        let (hpos', _) = _position stella
        resmp0' <- liftIO $ fastGetORegister r resmp0
        resmp1' <- liftIO $ fastGetORegister r resmp1
        when (testBit resmp0' 1) $ mpos0 .= hpos'
        when (testBit resmp1' 1) $ mpos1 .= hpos'

        liftIO $ do
            !final <- compositeAndCollide stella pixelx hpos' r
            --print final
            pokeElemOff ptr' (fromIntegral i) (lut!(final `shift` (-1)))

    position %= updatePos

    stellaTick (n-1)


stellaTickUntil :: Int64 -> MonadAtari ()
stellaTickUntil n = do
    c <- use stellaClock
    stellaTick (fromIntegral (n-c))

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
-}

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
stellaDebugStr :: (MonadIO m, MonadState Atari2600 m) =>
                  Int -> String -> m ()
stellaDebugStr n str = do
    d <- use (stellaDebug . debugLevel)
    if n <= d
        then do
            liftIO $ putStr str
        else return ()

{- INLINE stellaDebugStrLn -}
stellaDebugStrLn :: (MonadIO m, MonadState Atari2600 m) =>
                    Int -> String -> m ()
stellaDebugStrLn n str = do
    d <- use (stellaDebug . debugLevel)
    if n <= d
        then do
            liftIO $ putStrLn str
        else return ()

{-# INLINE wrap160 #-}
wrap160 :: CInt -> CInt
wrap160 i | i < picx = wrap160 (i+160)
          | i >= picx+160 = wrap160 (i-160)
wrap160 i = i

{-# INLINE clockMove #-}
clockMove :: Word8 -> CInt
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

{- INLINE stellaVblank -}
stellaVblank :: Word8 -> MonadAtari ()
stellaVblank v = do
    stellaDebugStrLn 0 $ "VBLANK " ++ showHex v ""
    --vold <- getORegister vblank
    --vold <- use vblank
    -- Set latches for INPT4 and INPT5
    when (testBit v 6) $ do
        i <- getIRegister inpt4 -- XXX write modifyIRegister
        putIRegister inpt4 (setBit i 7)
        i <- getIRegister inpt5
        putIRegister inpt5 (setBit i 7)

    --vblank .= v
    putORegister vblank v

{-# INLINE hpos #-}
{-# INLINE vpos #-}
hpos, vpos :: Lens' Atari2600 CInt
hpos = position . _1
vpos = position . _2

{-# INLINE ppos0 #-}
{-# INLINE ppos1 #-}
{-# INLINE mpos0 #-}
{-# INLINE mpos1 #-}
{-# INLINE bpos #-}
ppos0, ppos1, mpos0, mpos1, bpos :: Lens' Atari2600 CInt
ppos0 = sprites . s_ppos0
ppos1 = sprites . s_ppos1
mpos0 = sprites . s_mpos0
mpos1 = sprites . s_mpos1
bpos = sprites . s_bpos

{-# INLINABLE updatePos #-}
updatePos :: (CInt, CInt) -> (CInt, CInt)
updatePos (hpos0, vpos0) =
    let hpos' = hpos0+1
    in if hpos' < picx+160
        then (hpos', vpos0)
        else let vpos' = vpos0+1
             in if vpos' < picy+192
                then (0, vpos')
                else (0, 0)

{- INLINE compositeAndCollide -}
compositeAndCollide :: Atari2600 -> CInt -> CInt -> IOUArray OReg Word8 -> IO Word8
compositeAndCollide stella pixelx hpos' r = do
    resmp0' <- fastGetORegister r resmp0
    resmp1' <- fastGetORegister r resmp1

    let ir = stella ^. iregisters
    !ctrlpf' <- fastGetORegister r ctrlpf
    !colupf' <- fastGetORegister r colupf
    !colup0' <- fastGetORegister r colup0
    !colup1' <- fastGetORegister r colup1
    !colubk' <- fastGetORegister r colubk
    let !playfieldColour = if testBit ctrlpf' 1
            then if pixelx < 80
                then colup0'
                else colup1'
            else colupf' -- does ball get this too?

    let graphics' = stella ^. graphics
    let sprites' = stella ^. sprites
    -- XXX Side effects in missile0/1
    !enam0' <- fastGetORegister r enam0
    !enam1' <- fastGetORegister r enam1
    !nusiz0' <- fastGetORegister r nusiz0
    !nusiz1' <- fastGetORegister r nusiz1
    let !lmissile0 = missile nusiz0' enam0' hpos' (sprites' ^. s_mpos0) resmp0'
    let !lmissile1 = missile nusiz1' enam1' hpos' (sprites' ^. s_mpos1) resmp1'
    !lplayer0 <- player0 r graphics' nusiz0' hpos' (sprites' ^. s_ppos0)
    !lplayer1 <- player1 r graphics' nusiz1' hpos' (sprites' ^. s_ppos1)
    !lball <- ball graphics' ctrlpf' hpos' (sprites' ^. s_bpos)
    !lplayfield <- playfield r ctrlpf' (fromIntegral $ pixelx `div` 4)

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

-- Atari2600 programmer's guide p.22
{- INLINE missile0 -}
-- XXX Note that this updates mpos0 so need to take into account XXX
missile :: Word8 -> Word8 -> CInt -> CInt -> Word8 -> Bool
missile nusiz0' enam0' hpos' mpos0' resmp0' =
    if testBit resmp0' 1
        then False
        else if testBit enam0' 1
            then 
                let o = hpos'-mpos0'
                in o >= 0 && o < missileSize nusiz0'
            else False

-- Atari2600 programmer's guide p.40
{- INLINE player0 -}
player0 :: IOUArray OReg Word8 -> Graphics -> Word8 -> CInt -> CInt -> IO Bool
player0 r graphics' nusiz0' hpos' ppos0' = do
    let o = hpos'-ppos0'
    sizeCopies <- (0b111 .&.) <$> fastGetORegister r nusiz0
    let !delayP0' = graphics' ^. delayP0
    let !grp0' = if delayP0'
        then graphics' ^.oldGrp0
        else graphics' ^. newGrp0
    refp0' <- fastGetORegister r refp0
    return $ stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: IOUArray OReg Word8 -> Graphics -> Word8 -> CInt -> CInt -> IO Bool
player1 r graphics' nusiz1' hpos' ppos1' = do
    let o = hpos'-ppos1'
    sizeCopies <- (0b111 .&.) <$> fastGetORegister r nusiz1
    let !delayP1' = graphics' ^. delayP1
    let !grp1' = if delayP1'
        then graphics' ^. oldGrp1
        else graphics' ^. newGrp1
    refp1' <- fastGetORegister r refp1
    return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

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

{- INLINE playfield -}
playfield :: IOUArray OReg Word8 -> Word8 -> Int -> IO Bool
playfield r ctrlpf' i | i >= 0 && i < 4 = flip testBit (i+4) <$> fastGetORegister r pf0
                      | i >=4 && i < 12 = flip testBit (11-i) <$> fastGetORegister r pf1
                      | i >= 12 && i < 20 = flip testBit (i-12) <$> fastGetORegister r pf2
playfield r ctrlpf' i | i >= 20 && i < 40 = playfield r ctrlpf' $ if testBit ctrlpf' 0 then 39-i else i-20

missileSize :: Word8 -> CInt
missileSize nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

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

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True x = x
flipIf False x = 7-x
