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
    readBinary romArray "acorn_roms/Atom_Kernel.rom" (0xf000 - 0xc000)
    readBinary romArray "acorn_roms/Atom_Basic.rom" (0xc000 - 0xc000)
    readBinary romArray "acorn_roms/Atom_FloatingPoint.rom" (0xd000 - 0xc000)

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
                handleKey atariKeys motion key
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
