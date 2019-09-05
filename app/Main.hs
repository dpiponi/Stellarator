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
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Array.IO
#if TRACE
import Data.Array.Storable
#endif
import Data.Binary hiding (get)
import Debugger
import Delays
import Display
import Emulation
import Events
import Graphics.UI.GLFW
import Keys
import Memory
import Metrics
import Prelude hiding (last, init, null)
import Stella
import Step
import System.Console.CmdArgs hiding ((+=))
import System.Exit

data Args = Args { file :: String, bank :: String,
                   options :: String,
                   debugStart :: Bool } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin",
                bank = "",
                options = ".stellarator-options",
                debugStart = False }

loopEmulation :: AtariKeys -> TQueue UIKey -> MonadAtari ()
loopEmulation atariKeys queue = do
--     liftIO pollEvents
    maybeKey <- liftIO $ atomically $ tryReadTQueue queue
    case maybeKey of
        Nothing -> return ()
        Just queuedKey -> do
            let UIKey {uiKey = key, uiState = motion} = queuedKey
            handleKey atariKeys motion key
    stellaClock' <- useStellaClock id
    loopUntil (stellaClock' + 1000)

    loopEmulation atariKeys queue

startingState :: Args -> Options -> Window -> IO Atari2600
startingState args' options' window = do
    let alpha = motionBlurAlpha options'
    (prog, attrib, tex', lastTex', textureData', lastTextureData') <- initResources alpha

    romArray <- newArray (0, 0x7fff) 0 :: IO (IOUArray Int Word8)
    ramArray <- newArray (0, 0x7f) 0 :: IO (IOUArray Int Word8)
#if TRACE
    recordArray <- newArray (0, 2^(24 :: Int)-1) 0 :: IO (StorableArray Int Word8)
#endif
    bankStyle <- readBinary romArray (file args') 0x0000
    let bankStyle' = bankStyleByName bankStyle (bank args')
    let controllerTypeString = controllerTypes options'
    let controllerType = read controllerTypeString

    let initBankState = initialBankState bankStyle'
    print $ "Initial bank state = " ++ show initBankState
    let screenScale' = screenScale options'
    initState screenScale'
              (screenWidth*fst screenScale') (screenHeight*snd screenScale')
              ramArray
#if TRACE
              recordArray
#endif
              initBankState romArray
              0x0000 window prog attrib tex' lastTex' textureData' lastTextureData' delayList
              controllerType

keyCallback :: TQueue UIKey ->
               Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback queue _window key someInt state mods = 
            atomically $ writeTQueue queue (UIKey key someInt state mods)

main :: IO ()
main = do
    args' <- cmdArgs clargs

    let optionsFile = options args'
    putStrLn $ "Reading options from '" ++ optionsFile ++ "'"
    putStrLn $ "Debug = " ++ show (debugStart args')
    optionsString <- readFile optionsFile
    let options' = read optionsString :: Options
    print options'
    let screenScale' = screenScale options'
    -- XXX Make list of default keys
    let Just atariKeys = keysFromOptions options'

    rc <- init -- init video
    when (not rc) $ die "Couldn't init graphics"
    queue <- newTQueueIO
    window <- makeMainWindow screenScale'
    setKeyCallback window (Just $ keyCallback queue)
    void $ setWindowCloseCallback window $ Just $ \_ -> exitSuccess

    state <- startingState args' options' window

    void $ forkIO $ forever pollEvents

    void $ with2600 state $ do
        initHardware
        when (debugStart args') runDebugger
        resetNextFrame
        loopEmulation atariKeys queue

    destroyWindow window
    -- XXX Free malloced data?
    terminate
