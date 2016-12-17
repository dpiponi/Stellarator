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
import Emulation
import Control.Lens hiding (_last)
import Control.Monad
import Text.Parsec
import Control.Monad.State.Strict
import Metrics
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
import VideoOps
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
import Memory
import Stella.Graphics
import Stella.Sprites
import DebugState
import Atari2600
import Debugger


--  XXX Do this! If reset occurs during horizontal blank, the object will appear at the left side of the television screen
data Args = Args { file :: String, bank :: BankMode } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin", bank = UnBanked }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times !n m = m >> times (n-1) m

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
            hardware . trigger1 .= pressed
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

loopUntil :: Int64 -> MonadAtari ()
loopUntil !n = do
    !stellaClock' <- use (hardware . stellaClock)
    when (stellaClock' < n) $ do
        step
        loopUntil n

main :: IO ()
main = do
    args <- cmdArgs clargs
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Stellarator"
                               SDL.defaultWindow {
                                    SDL.windowInitialSize = V2 (fromIntegral $ xscale*screenWidth)
                                                               (fromIntegral $ yscale*screenHeight) }
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window

    backSurface <- createRGBSurface (V2 (fromIntegral screenWidth)
                                        (fromIntegral screenHeight)) RGB888

    rom <- newArray (0, 0x3fff) 0 :: IO (IOUArray Int Word8)
    ram <- newArray (0, 0x7f) 0 :: IO (IOUArray Int Word8)
    readBinary rom (file args) 0x0000
    pclo <- readArray rom 0x0ffc
    pchi <- readArray rom 0x0ffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)

    oregs <- newArray (0, 0x3f) 0
    iregs <- newArray (0, 0x300) 0 -- XXX no need for that many really
    let style = bank args
    let state = initState ram style rom oregs iregs
                          initialPC backSurface screenSurface window

    let loop = do
            events <- liftIO $ SDL.pollEvents

            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            forM_ events handleEvent
            stellaClock' <-  use (hardware . stellaClock)
            loopUntil (stellaClock' + 1000)

            loop

    flip runS state $ unM $ do
        -- Joystick buttons not pressed
        putIRegister inpt4 0x80
        putIRegister inpt5 0x80
        putIRegister swcha 0b11111111
        putIRegister swchb 0b00001011
        loop

    SDL.destroyWindow window
    SDL.freeSurface backSurface
    SDL.quit
