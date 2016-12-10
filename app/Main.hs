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
import DebugState
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
import Stella.IntervalTimer
import Stella.TIARegisters
import Stella.SDLState

import DebugCmd
import MemoryMap
import Stella.Graphics
import Stella.Sprites
import DebugState
import Atari2600

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

dumpStella :: MonadAtari ()
dumpStella = do
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
setBreak :: (MonadIO m, MonadState Atari2600 m) =>
               CInt -> CInt -> m ()
setBreak x y = stellaDebug . posbreak .= (x+picx, y+picy)

graphicsDelay :: Int64 -> MonadAtari ()
graphicsDelay n = do
    c <- use clock
    stellaTickUntil (3*c+n)

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
       otherwise -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

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

{-
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
-}

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
            setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)
        MouseMotionEvent
            (MouseMotionEventData win device [ButtonLeft] pos rel) -> do
            liftIO $ print pos
            let P (V2 x y) = pos
            setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)
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
        SDL.ScancodeUp ->  modifyIRegister swcha (setBitTo 4 (not pressed))
        SDL.ScancodeDown ->  modifyIRegister swcha (setBitTo 5 (not pressed))
        SDL.ScancodeLeft ->  modifyIRegister swcha (setBitTo 6 (not pressed))
        SDL.ScancodeRight ->  modifyIRegister swcha (setBitTo 7 (not pressed))
        SDL.ScancodeC ->  modifyIRegister swchb (setBitTo 1 (not pressed))
        SDL.ScancodeV ->  modifyIRegister swchb (setBitTo 0 (not pressed))
        SDL.ScancodeSpace ->  do
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

{-
initState :: IOUArray Int Word8 ->
             IOUArray OReg Word8 ->
             IOUArray IReg Word8 ->
             Word16 ->
             Surface -> Surface ->
             SDL.Window -> Atari2600
initState memory oregs iregs initialPC helloWorld screenSurface window = Atari2600 {
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
      _stellaDebug = DebugState.start
  }
  -}

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
    let state = initState UnBanked memory oregs iregs initialPC helloWorld screenSurface window

    let loopUntil n = do
            stellaClock' <-  use stellaClock
            when (stellaClock' < n) $ do
                step
                loopUntil n

    --SDL.setHintWithPriority SDL.NormalPriority SDL.HintRenderVSync SDL.EnableVSync
    -- https://hackage.haskell.org/package/sdl2-2.1.3

    let loop = do
            events <- liftIO $ SDL.pollEvents

            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            forM_ events handleEvent
            stellaClock' <-  use stellaClock
            loopUntil (stellaClock' + 1000)

            loop

    flip runStateT state $ unM $ do
        -- Joystick buttons not pressed
        putIRegister inpt4 0x80
        putIRegister inpt5 0x80
        putIRegister swcha 0b11111111
        putIRegister swchb 0b00001011
        loop

    SDL.destroyWindow window
    SDL.freeSurface helloWorld
    SDL.quit
