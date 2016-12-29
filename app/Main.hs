{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Graphics.Rendering.OpenGL
import Asm
import Atari2600
import Binary
import Control.Applicative
import Control.Concurrent
import Control.Concurrent
import Display
import Control.Concurrent (threadDelay)
import Control.Lens hiding (_last)
import Control.Monad
import Control.Monad.Reader
import Core
import Data.Array.IO
import Data.Array.Unboxed
import Data.Binary hiding (get)
import Data.Binary.Get
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.ByteString as B hiding (last, putStr, putStrLn, getLine, length, elem, map, reverse, readFile)
import Data.Char
import Data.IORef
import Data.Int
import Data.Int(Int16)
import Data.Monoid
import Data.Vector.Storable.Mutable as V hiding (modify, readFile, read)
import Data.Word
import Debug.Trace
import DebugCmd
import DebugState
import Debugger
import Disasm
import Emulation
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Memory
import Metrics
import Numeric
import Prelude hiding (last)
import SDL.Audio
import SDL.Event
import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import System.Console.CmdArgs hiding ((+=))
import System.Console.Haskeline
import System.Exit
import System.IO
import System.Random
import TIAColors
import Text.Parsec
import VideoOps
import Keys
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.Map.Strict as Map
import qualified SDL

--  XXX Do this If reset occurs during horizontal blank, the object will appear at the left side of the television screen
data Args = Args { file :: String, bank :: String, options :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin", bank = "", options = ".stellarator-options" }

{- INLINE isPressed -}
isPressed :: InputMotion -> Bool
isPressed Pressed = True
isPressed Released = False

handleEvent :: [(Scancode, AtariKey)] -> EventPayload -> MonadAtari ()

handleEvent atariKeys (MouseButtonEvent (MouseButtonEventData win Pressed device ButtonLeft clicks pos)) = do
    liftIO $ print pos
    let P (V2 x y) = pos
    setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)

handleEvent atariKeys (MouseMotionEvent (MouseMotionEventData win device [ButtonLeft] pos rel)) = do
    liftIO $ print pos
    let P (V2 x y) = pos
    setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)

handleEvent atariKeys (KeyboardEvent (KeyboardEventData win motion rep sym)) = handleKey atariKeys motion sym

handleEvent _ _ = return ()

trigger1Pressed :: Bool -> MonadAtari ()
trigger1Pressed pressed = do
    store trigger1 pressed
    vblank' <- load vblank
    let latch = testBit vblank' 6
    case (latch, pressed) of
        (False, _   ) -> modify inpt4 $ bitAt 7 .~ not pressed
        (True, False) -> return ()
        (True, True ) -> modify inpt4 $ bitAt 7 .~ False

handleKey :: [(Scancode, AtariKey)] -> InputMotion -> Keysym -> MonadAtari ()
handleKey atariKeys motion sym = do
    let scancode = keysymScancode sym
    let mAtariKey = lookup scancode atariKeys
    case mAtariKey of
        Nothing -> return ()
        Just atariKey -> do
            let pressed = isPressed motion
            case atariKey of
                Joystick1Up      -> modify swcha $ bitAt 4 .~ not pressed
                Joystick1Down    -> modify swcha $ bitAt 5 .~ not pressed
                Joystick1Left    -> modify swcha $ bitAt 6 .~ not pressed
                Joystick1Right   -> modify swcha $ bitAt 7 .~ not pressed
                Joystick1Trigger -> trigger1Pressed pressed
                GameSelect       -> modify swchb $ bitAt 1 .~ not pressed
                GameReset        -> modify swchb $ bitAt 0 .~ not pressed
                DumpState        -> Emulation.dumpState
                GameQuit         -> liftIO $ exitSuccess
                EnterDebugger    -> when pressed $ do
                                        t <- liftIO $ forkIO $ let spin = SDL.pollEvents >> spin in spin
                                        Emulation.dumpState
                                        runDebugger
                                        liftIO $ killThread t

loopUntil :: Int64 -> MonadAtari ()
loopUntil n = do
    stellaClock' <- useStellaClock id
    when (stellaClock' < n) $ step >> loopUntil n

main :: IO ()
main = do
    args <- cmdArgs clargs

    let optionsFile = options args
    putStrLn $ "Reading options from '" ++ optionsFile ++ "'"
    optionsString <- readFile optionsFile
    let options' = read optionsString :: Options
    print options'
    let atariKeys = keysFromOptions options'

    --SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    SDL.initializeAll
    window <- SDL.createWindow "Stellarator"
                               SDL.defaultWindow {
                                    SDL.windowInitialSize = V2 (fromIntegral $ xscale*screenWidth)
                                                               (fromIntegral $ yscale*screenHeight) }
    --context <- SDL.glCreateContext window
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
    --SDL.swapInterval $= SDL.ImmediateUpdates
    (prog, attrib, tex, textureData) <- initResources

    rom <- newArray (0, 0x3fff) 0 :: IO (IOUArray Int Word8)
    ram <- newArray (0, 0x7f) 0 :: IO (IOUArray Int Word8)
    bankStyle <- readBinary rom (file args) 0x0000
    let bankStyle' = case (bank args) of
                        "f8" -> ModeF8
                        "f6" -> ModeF6
                        "3f" -> Mode3F
                        _    -> bankStyle

    let initBankState = case bankStyle' of
                            UnBanked -> NoBank
                            ModeF8 -> BankF8 0x0000
                            ModeF6 -> BankF6 0x0000
                            Mode3F -> Bank3F 0x0000
    print $ "Initial bank state = " ++ show initBankState

    --let style = bank args
    state <- initState ram initBankState rom 0x0000 window prog attrib tex textureData

    let loop = do
            events <- liftIO $ SDL.pollEvents

            --let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            forM_ events $ \event -> handleEvent atariKeys (eventPayload event)
            stellaClock' <- useStellaClock id
            loopUntil (stellaClock' + 1000)

            loop

    _ <- flip runReaderT state $ unM $ do
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
            -- runDebugger
            loop

    SDL.destroyWindow window
    --SDL.freeSurface backSurface
    -- XXX Free malloced data?
    SDL.quit
