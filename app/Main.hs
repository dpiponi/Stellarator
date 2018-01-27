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
import Display
import Control.Monad
import Control.Monad.Reader
import Core
import Data.Array.IO
import Data.Binary hiding (get)
import Data.Bits hiding (bit)
import Data.Int(Int64)
#if TRACE
import Data.Array.Storable
#endif
import Emulation
import Memory
import Metrics
import Numeric
import Prelude hiding (last)
import SDL.Event
import SDL.Vect
import System.Console.CmdArgs hiding ((+=))
import Keys
import qualified SDL
import Events
import Debugger

data Args = Args { file :: String, bank :: String, options :: String, debugStart :: Bool } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin", bank = "", options = ".stellarator-options", debugStart = False }

loopUntil :: Int64 -> MonadAtari ()
loopUntil n = do
    stellaClock' <- useStellaClock id
    when (stellaClock' < n) $ (pc @-> pcStep) >> step >> loopUntil n

--initHardware :: MonadAtari
--initHardware = do

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
    recordArray <- newArray (0, 2^24-1) 0 :: IO (StorableArray Int Word8)
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
                       0x0000 window prog attrib tex' textureData'

    let loop = do
            events <- liftIO $ SDL.pollEvents

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
            when (debugStart args') runDebugger
            loop

    SDL.destroyWindow window
    -- XXX Free malloced data?
    SDL.quit
