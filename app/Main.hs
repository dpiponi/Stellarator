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


--  XXX Do this! If reset occurs during horizontal blank, the object will appear at the left side of the television screen
data Args = Args { file :: String, bank :: BankMode } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin", bank = UnBanked }

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
            trigger1 .= pressed
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

main :: IO ()
main = do
    args <- cmdArgs clargs
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Stellarator" SDL.defaultWindow { SDL.windowInitialSize = V2 (xscale*screenWidth) (yscale*screenHeight) }
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window

    helloWorld <- createRGBSurface (V2 screenWidth screenHeight) RGB888

    rom <- newArray (0, 0x1fff) 0 :: IO (IOUArray Int Word8)
    ram <- newArray (0, 0x7f) 0 :: IO (IOUArray Int Word8)
    readBinary rom (file args) 0x0000
    pclo <- readArray rom 0x0ffc
    pchi <- readArray rom 0x0ffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)

    oregs <- newArray (0, 0x3f) 0
    --iregs <- newArray (0, 0x0d) 0
    iregs <- newArray (0, 0x300) 0 -- XXX no need for that many really
    let style = bank args
    let state = initState ram style rom oregs iregs
                          initialPC helloWorld screenSurface window

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
            loopUntil (stellaClock' + 250)

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
