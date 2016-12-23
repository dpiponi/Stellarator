{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Atari2600
import Binary
import Control.Applicative
import Control.Concurrent
import Control.Concurrent (threadDelay)
import Control.Lens hiding (_last)
import Control.Monad
import Control.Monad.Reader
--import Control.Monad.State.Strict
import Core
import Data.Array.IO
import Data.Array.Unboxed
import Data.Binary hiding (get)
import Data.Binary.Get
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.ByteString as B hiding (last, putStr, putStrLn, getLine, length, elem, map, reverse)
import Data.Char
import Data.Int
import Data.Monoid
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
import SDL.Event
import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import SDL.Audio
import Asm
import System.Console.CmdArgs hiding ((+=))
import System.Console.Haskeline
import System.Exit
import System.IO
import System.Random
import TIAColors
import Text.Parsec
import VideoOps
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.Map.Strict as Map
import qualified SDL
import Data.IORef
import Control.Concurrent
import Data.Int(Int16)
import Data.Vector.Storable.Mutable as V hiding (modify)

--  XXX Do this If reset occurs during horizontal blank, the object will appear at the left side of the television screen
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

handleEvent :: EventPayload -> MonadAtari ()

handleEvent (MouseButtonEvent (MouseButtonEventData win Pressed device ButtonLeft clicks pos)) = do
    liftIO $ print pos
    let P (V2 x y) = pos
    setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)

handleEvent (MouseMotionEvent (MouseMotionEventData win device [ButtonLeft] pos rel)) = do
    liftIO $ print pos
    let P (V2 x y) = pos
    setBreak (fromIntegral x `div` xscale) (fromIntegral y `div` yscale)

handleEvent (KeyboardEvent (KeyboardEventData win motion rep sym)) = handleKey motion sym

handleEvent _ = return ()

trigger1Pressed :: Bool -> MonadAtari ()
trigger1Pressed pressed = do
    store trigger1 pressed
    vblank' <- load vblank
    let latch = testBit vblank' 6
    case (latch, pressed) of
        (False, _) -> do
            inpt4' <- load inpt4
            store inpt4 ((clearBit inpt4' 7) .|. bit 7 (not pressed))
        (True, False) -> return ()
        (True, True) -> do
            inpt4' <- load inpt4
            store inpt4 (clearBit inpt4' 7)

handleKey :: InputMotion -> Keysym -> MonadAtari ()
handleKey motion sym = do
    let pressed = isPressed motion
    case keysymScancode sym of
        SDL.Scancode1      -> dumpState
        SDL.ScancodeUp     -> modify swcha $ bitAt 4 .~ not pressed
        SDL.ScancodeDown   -> modify swcha $ bitAt 5 .~ not pressed
        SDL.ScancodeLeft   -> modify swcha $ bitAt 6 .~ not pressed
        SDL.ScancodeRight  -> modify swcha $ bitAt 7 .~ not pressed
        SDL.ScancodeC      -> modify swchb $ bitAt 1 .~ not pressed
        SDL.ScancodeV      -> modify swchb $ bitAt 0 .~ not pressed
        SDL.ScancodeSpace  -> trigger1Pressed pressed
        SDL.ScancodeQ      -> liftIO $ exitSuccess
        SDL.ScancodeEscape -> when pressed $ do
            t <- liftIO $ forkIO $ let spin = SDL.pollEvents >> spin in spin
            dumpState
            runDebugger
            liftIO $ killThread t
        otherwise -> return ()

loopUntil :: Int64 -> MonadAtari ()
loopUntil n = do
    stellaClock' <- useStellaClock id
    when (stellaClock' < n) $ do
        step
        loopUntil n

{-
sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 48000 :: Double
             freq = 440 * 4
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (t * freq)))
      [0 :: Integer ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         sequence_ (Prelude.zipWith (write buffer)
                            [0 ..]
                            (Prelude.take n samples'))
         writeIORef samples
                    (Prelude.drop n samples')
    _ -> error "Unsupported audio format"
-}

main :: IO ()
main = do
    args <- cmdArgs clargs
    --SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    SDL.initializeAll
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

    let style = bank args
    state <- initState ram style rom initialPC backSurface screenSurface window

{-
    samples <- newIORef sinSamples
    (device, _) <- SDL.openAudioDevice OpenDeviceSpec {
        SDL.openDeviceFreq = Mandate 48000,
        SDL.openDeviceFormat = Mandate Signed16BitNativeAudio,
        SDL.openDeviceChannels = Mandate Mono,
        SDL.openDeviceSamples = 4096 * 2,
        SDL.openDeviceCallback = audioCB samples,
        SDL.openDeviceUsage = ForPlayback,
        SDL.openDeviceName = Nothing
    }
    setAudioDevicePlaybackState device Play
    threadDelay 1000000
-}

    let loop = do
            events <- liftIO $ SDL.pollEvents

            let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
            forM_ events (handleEvent . eventPayload)
            stellaClock' <- useStellaClock id
            loopUntil (stellaClock' + 1000)

            loop

    flip runReaderT state $ unM $ do
        -- Joystick buttons not pressed
        store inpt4 0x80
        store inpt5 0x80
        store swcha 0b11111111
        store swchb 0b00001011
        loop

    SDL.destroyWindow window
    SDL.freeSurface backSurface
    SDL.quit
