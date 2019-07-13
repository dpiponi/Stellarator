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
--     let Just atariKeys = keysFromOptions options'
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

    romArray <- newArray (0, 0x5fff) 0 :: IO (IOUArray Int Word8)
    ramArray <- newArray (0, 0x9fff) 0 :: IO (IOUArray Int Word8)
    readBinary romArray "acorn_roms/Atom_Kernel.rom" (0xf000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_Basic.rom" (0xc000 - 0xa000)
    readBinary romArray "acorn_roms/Atom_FloatingPoint.rom" (0xd000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_pcharme.rom" (0xa000 - 0xa000)
--     readBinary romArray "acorn_roms/Atom_Toolkit.rom" (0xa000 - 0xa000)
    readBinary romArray "utility.bin" (0xa000 - 0xa000)
--     readBinary ramArray "software/BB/PINBALL" (0x2900-22)
--     readBinary ramArray "software/BB/GALAXBB" (0x2900-22)
--     readBinary ramArray "software/AS/ADVENT/ADVENTUR" (0x2900-22)
--     readBinary ramArray "software/BB/INVADBB" (0x2900-22)
--     readBinary ramArray "software/BB/LUNARBB" (0x2900-22)
--     readBinary ramArray "software/L9/DUNGEON/DUNGEON" (0xe00-22)
--     readBinary ramArray "software/L9/DUNGEON/DUNGEON" (0x400-22)
--     readBinary ramArray "JSW2CODE" (0x400-22)
--     readBinary ramArray "elite.atm" (0x400-22)
--     readBinary ramArray "software/BB/CHESSBB" (0x2900-22)
--     readBinary ramArray "software/BB/INVADBB" (0x2900-22)
--     readBinary ramArray "acorn_roms/Atom_Demo.rom" (0x2900)

    state <- initState screenScaleX' screenScaleY'
                       (screenWidth*screenScaleX') (screenHeight*screenScaleY')
                       ramArray
                       romArray
                       0x0000 window prog attrib tex' lastTex' textureData' lastTextureData'
                       controllerType

    let loop = do
            liftIO pollEvents
            queue <- liftIO $ readIORef queueRef
            when (not (null queue)) $ do
                let Just (queuedKey, queue') = popFront queue
--                 liftIO $ print queue
                liftIO $ writeIORef queueRef queue'
                let UIKey {uiKey = key, uiState = motion} = queuedKey
                handleKey motion key
            loopUntil 1000

            loop

    _ <- flip runReaderT state $ unM $ do
            initHardware
            when (debugStart args') runDebugger
            resetNextFrame
            loop

    destroyWindow window
    -- XXX Free malloced data?
    terminate
