{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

--import Graphics.Rendering.OpenGL
import Atari2600
import Binary
import Display
import Control.Monad
import Control.Monad.Reader
import Data.Array.IO
import Data.Binary hiding (get)
#if TRACE
import Data.Array.Storable
#endif
import Emulation
import Memory
import Metrics
import Prelude hiding (last)
import SDL.Event
--import SDL.Vect
import System.Console.CmdArgs hiding ((+=))
import Keys
import qualified SDL
import Events
import Debugger

data Args = Args { file :: String, bank :: String, options :: String, debugStart :: Bool } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin",
                bank = "",
                options = ".stellarator-options",
                debugStart = False }

delayList :: [(Word16, Int)]
delayList =  [
                (0x00, 0), -- VSYNC
                (0x01, 0), -- VBLANK
                (0x02, 0), -- WSYNC
                (0x03, 0), -- NUSIZ0
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
            ]

main :: IO ()
main = do
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

    SDL.initialize [SDL.InitVideo] --, SDL.InitAudio]
    window <- makeMainWindow screenScaleX' screenScaleY'

    (prog, attrib, tex', textureData') <- initResources

    romArray <- newArray (0, 0x7fff) 0 :: IO (IOUArray Int Word8)
    ramArray <- newArray (0, 0x7f) 0 :: IO (IOUArray Int Word8)
#if TRACE
    recordArray <- newArray (0, 2^(24 :: Int)-1) 0 :: IO (StorableArray Int Word8)
#endif
    bankStyle <- readBinary romArray (file args') 0x0000
    let bankStyle' = bankStyleByName bankStyle (bank args')

    let initBankState = initialBankState bankStyle'
    print $ "Initial bank state = " ++ show initBankState

    --let style = bank args
    state <- initState screenScaleX' screenScaleY'
                       (screenWidth*screenScaleX') (screenHeight*screenScaleY')
                       ramArray
#if TRACE
                       recordArray
#endif
                       initBankState romArray
                       0x0000 window prog attrib tex' textureData' delayList

    let loop = do
            events <- liftIO $ SDL.pollEvents

            forM_ events $ \event -> handleEvent atariKeys (eventPayload event)
            stellaClock' <- useStellaClock id
            loopUntil (stellaClock' + 1000)

            loop

    _ <- flip runReaderT state $ unM $ do
            initHardware
            when (debugStart args') runDebugger
            loop

    SDL.destroyWindow window
    -- XXX Free malloced data?
    SDL.quit
