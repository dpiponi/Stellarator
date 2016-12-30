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

data Args = Args { file :: String, bank :: String, options :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin", bank = "", options = ".stellarator-options" }

loopUntil :: Int64 -> MonadAtari ()
loopUntil n = do
    stellaClock' <- useStellaClock id
    when (stellaClock' < n) $ step >> loopUntil n

main :: IO ()
main = do
    args' <- cmdArgs clargs

    let optionsFile = options args'
    putStrLn $ "Reading options from '" ++ optionsFile ++ "'"
    optionsString <- readFile optionsFile
    let options' = read optionsString :: Options
    print options'
    let screenScaleX' = screenScaleX options'
    let screenScaleY' = screenScaleY options'
    -- XXX Make list of default keys
    let Just atariKeys = keysFromOptions options'

    --SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    SDL.initializeAll
    window <- SDL.createWindow "Stellarator"
                               SDL.defaultWindow {
                                    SDL.windowInitialSize = V2 (fromIntegral $ screenScaleX'*screenWidth)
                                                               (fromIntegral $ screenScaleY'*screenHeight) }
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
    (prog, attrib, tex', textureData') <- initResources

    romArray <- newArray (0, 0x3fff) 0 :: IO (IOUArray Int Word8)
    ramArray <- newArray (0, 0x7f) 0 :: IO (IOUArray Int Word8)
    bankStyle <- readBinary romArray (file args') 0x0000
    let bankStyle' = case (bank args') of
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
    state <- initState screenScaleX' screenScaleY'
                       (screenWidth*screenScaleX') (screenHeight*screenScaleY')
                       ramArray initBankState romArray
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
            -- runDebugger
            loop

    SDL.destroyWindow window
    -- XXX Free malloced data?
    SDL.quit
