{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (last, init, null)
import AcornAtom
import Binary
import Control.Monad
import Control.Monad.Reader
import Data.Array.IO
import Data.Binary hiding (get)
import Debugger
import System.IO
import Data.Char
import System.Exit
import Display
import Emulation
import Events
import Keys
import Metrics
import Stella
-- import SDL.Event
import Step
import System.Console.CmdArgs hiding ((+=))
-- import qualified SDL
import Graphics.UI.GLFW
import Data.IORef
import Data.Dequeue
import Sound.ProteaAudio
#if TRACE
import Data.Array.Storable
#endif

data Args = Args { file :: String, options :: String, debugStart :: Bool } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin",
                options = ".stellarator-options",
                debugStart = False }

delayList :: [(Word16, Int)]
delayList =  [
#if 1
                -- My figures
                (0x00, 0), -- VSYNC
                (0x01, 0), -- VBLANK
                (0x02, 0), -- WSYNC
                (0x04, 4), -- NUSIZ0
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
#else
                -- From Stella
                (0x0b, 1), -- REFP0
                (0x0c, 1), -- REFP1
                (0x0d, 2), -- PF0
                (0x0e, 2), -- PF1
                (0x0f, 4), -- PF2
                (0x1b, 1), -- GRP0
                (0x1c, 1), -- GRP1
                (0x1d, 2), -- ENAM0
                (0x1e, 2), -- ENAM1
                (0x1f, 2), -- ENABL
                (0x20, 2), -- HMP0
                (0x21, 2), -- HMP1
                (0x22, 2), -- HMM0
                (0x23, 2), -- HMM1
                (0x24, 2), -- HMBL
                (0x2a, 6), -- HMOVE
                (0x2b, 2), -- HMCLR
                -- From Stella
                --(0x05, 4), -- NUSIZ1
                --(0x0d, 3), -- PF0
                --(0x0e, 3), -- PF1
                --(0x0f, 3), -- PF2
                (0x10, 5), -- RESP0
                (0x11, 5), -- RESP1
                (0x12, 4), -- RESM0
                (0x13, 4), -- RESM1
                (0x14, 4)  -- RESBL
                --(0x1b, 1), -- GRP0
                --(0x1c, 1)  -- GRP1
#endif
            ]

main :: IO ()
main = do
    fontData <- readFont "font.txt"
    args' <- cmdArgs clargs

    let optionsFile = options args'
    putStrLn $ "Reading options from '" ++ optionsFile ++ "'"
    putStrLn $ "Debug = " ++ show (debugStart args')
    optionsString <- readFile optionsFile
    let options' = read optionsString :: Options
    print options'
    let screenScaleX' = screenScaleX options'
    let screenScaleY' = screenScaleY options'
    -- XXX Make list of default keys
    let Just atariKeys = keysFromOptions options'
    let controllerTypeString = controllerTypes options'
    let controllerType = read controllerTypeString
    let alpha = motionBlurAlpha options'

    rc <- init -- init video
    when (not rc) $ die "Couldn't init graphics"
    queueRef <- newIORef empty
    window <- makeMainWindow screenScaleX' screenScaleY' queueRef

    -- init audio
    result <- initAudio 64 44100 1024
    unless result $ die "Couldn't init sound"

    (prog, attrib, tex', lastTex', textureData', lastTextureData') <- initResources alpha fontData

    romArray <- newArray (0, 0x3fff) 0 :: IO (IOUArray Int Word8)
    ramArray <- newArray (0, 0xbfff) 0 :: IO (IOUArray Int Word8)
#if TRACE
    recordArray <- newArray (0, 2^(24 :: Int)-1) 0 :: IO (StorableArray Int Word8)
#endif
    readBinary romArray "acorn_roms/Atom_Kernel.rom" (0xf000 - 0xc000)
    readBinary romArray "acorn_roms/Atom_Basic.rom" (0xc000 - 0xc000)

    state <- initState screenScaleX' screenScaleY'
                       (screenWidth*screenScaleX') (screenHeight*screenScaleY')
                       ramArray
#if TRACE
                       recordArray
#endif
                       romArray
                       0x0000 window prog attrib tex' lastTex' textureData' lastTextureData' delayList
                       controllerType

    let loop = do
            liftIO pollEvents
            queue <- liftIO $ readIORef queueRef
            when (not (null queue)) $ do
                let Just (queuedKey, queue') = popFront queue
--                 liftIO $ print queue
                liftIO $ writeIORef queueRef queue'
                let UIKey {uiKey = key, uiState = motion} = queuedKey
                handleKey atariKeys motion key
            stellaClock' <- useStellaClock id
            loopUntil (stellaClock' + 1000)

            loop

    _ <- flip runReaderT state $ unM $ do
            initHardware
            when (debugStart args') runDebugger
            resetNextFrame
            loop

    destroyWindow window
    -- XXX Free malloced data?
    terminate
