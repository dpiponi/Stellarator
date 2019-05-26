module Events where

-- import SDL.Event
-- import SDL.Input.Keyboard
import Keys hiding (debugMode)
import Atari2600
-- import SDL.Vect
import Emulation
-- import qualified SDL
import Control.Monad.Reader
import Control.Lens
import Asm
import Data.Bits
import Data.Bits.Lens
import System.Exit
import Control.Concurrent
import Metrics
import Data.Array.Storable
import Debugger
import Stella
import Graphics.UI.GLFW
import qualified Data.Map.Strict as M
#if TRACE
import System.IO
import Data.IORef
#endif

{- INLINE isPressed -}
isPressed :: KeyState -> Bool
isPressed KeyState'Pressed = True
isPressed KeyState'Repeating = True -- I don't know!
isPressed KeyState'Released = False

-- XXX Move to Stella?
{- INLINE setBreak -}
setBreak :: Int -> Int -> MonadAtari ()
setBreak breakX breakY = do
    xbreak @= (breakX+picx)
    ybreak @= (breakY+picy)

{-
-- Handle events later XXX
handleEvent :: AtariKeys -> EventPayload -> MonadAtari ()
handleEvent _ (MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ pos)) = do
    xscale' <- view xscale
    yscale' <- view yscale
    liftIO $ print pos
    let P (V2 x' y') = pos
    setBreak (fromIntegral x' `div` xscale') (fromIntegral y' `div` yscale')

handleEvent _ (MouseMotionEvent (MouseMotionEventData _ _ [ButtonLeft] pos _)) = do
    xscale' <- view xscale
    yscale' <- view yscale
    liftIO $ print pos
    let P (V2 x' y') = pos
    setBreak (fromIntegral x' `div` xscale') (fromIntegral y' `div` yscale')

handleEvent atariKeys (KeyboardEvent (KeyboardEventData _ motion _ sym)) = handleKey atariKeys motion sym

handleEvent _ _ = return ()
-}

doDelayUp :: MonadAtari ()
doDelayUp = do
    delays' <- view delays
    liftIO $ do
        d <- readArray delays' 0x1b
        writeArray delays' 0x1b (d+1)
        writeArray delays' 0x1c (d+1)
        putStrLn $ "delay = " ++ show (d+1)

doDelayDown :: MonadAtari ()
doDelayDown = do
    delays' <- view delays
    liftIO $ do
        d <- readArray delays' 0x1b
        writeArray delays' 0x1b (d-1)
        writeArray delays' 0x1c (d-1)
        putStrLn $ "delay = " ++ show (d-1)

trigger1Pressed :: Bool -> MonadAtari ()
trigger1Pressed pressed = do
    store trigger1 pressed
    vblank' <- load vblank
    let latch = testBit vblank' 6
    case (latch, pressed) of
        (False, _   ) -> modify inpt4 $ bitAt 7 .~ not pressed
        (True, False) -> return ()
        (True, True ) -> modify inpt4 $ bitAt 7 .~ False

trigger2Pressed :: Bool -> MonadAtari ()
trigger2Pressed pressed = do
    store trigger2 pressed
    vblank' <- load vblank
    let latch = testBit vblank' 6
    case (latch, pressed) of
        (False, _   ) -> modify inpt5 $ bitAt 7 .~ not pressed
        (True, False) -> return ()
        (True, True ) -> modify inpt5 $ bitAt 7 .~ False

-- XXX probably get ride of case
handleKey :: AtariKeys -> KeyState -> Key -> MonadAtari ()
handleKey atariKeys motion key = do
--     let scancode = keysymScancode sym
    let mAtariKey = M.lookup key atariKeys
    case mAtariKey of
        Nothing -> return ()
        Just atariKey -> do
            let pressed = isPressed motion
            case atariKey of
                -- http://atariage.com/forums/topic/247615-trying-to-figure-out-keyboard-controllers-inpt/
                KeyboardController i j -> store (kbd i j) $ isPressed motion
                Joystick1Up      -> modify swcha $ bitAt 4 .~ not pressed
                Joystick1Down    -> modify swcha $ bitAt 5 .~ not pressed
                Joystick1Left    -> modify swcha $ bitAt 6 .~ not pressed
                Joystick1Right   -> modify swcha $ bitAt 7 .~ not pressed
                Joystick2Up      -> modify swcha $ bitAt 0 .~ not pressed
                Joystick2Down    -> modify swcha $ bitAt 1 .~ not pressed
                Joystick2Left    -> modify swcha $ bitAt 2 .~ not pressed
                Joystick2Right   -> modify swcha $ bitAt 3 .~ not pressed
                Joystick1Trigger -> trigger1Pressed pressed
                Joystick2Trigger -> trigger2Pressed pressed
                TVType           -> modify swchb $ bitAt 3 .~ not pressed
                GameSelect       -> modify swchb $ bitAt 1 .~ not pressed
                GameReset        -> modify swchb $ bitAt 0 .~ not pressed
                DumpState        -> Emulation.dumpState
                GameQuit         -> liftIO $ exitSuccess
                EnterDebugger    -> when pressed $ do
                                        -- Throw away SDL events
                                        -- Rewrite as a withXXX XXX
--                                         t <- liftIO $ forkIO $ let spin = SDL.pollEvents >> spin in spin
                                        Emulation.dumpState
                                        runDebugger
--                                         liftIO $ killThread t
                                        resetNextFrame
                DebugMode        -> when pressed $ do
                                        modify debugColours not
                                        debugMode <- load debugColours
                                        when debugMode $ liftIO $ do
                                            print "COLUBK -- black"
                                            print "COLUPF -- white"
                                            print "COLUB  -- gray"
                                            print "COLUP0 -- dark red"
                                            print "COLUP1 -- dark blue"
                                            print "COLUM0 -- light red"
                                            print "COLUM1 -- light blue"
#if TRACE
                WriteRecord      -> when pressed $ do
                                        liftIO $ print "Write record!"
                                        atari <- ask
                                        let m = atari ^. record
                                        endPtr <- liftIO $ readIORef (atari ^. recordPtr)
                                        liftIO $ withStorableArray m $ \ptr -> do
                                            handle <- openBinaryFile "trace.record" WriteMode
                                            hPutBuf handle ptr endPtr
                                            hClose handle
#else
                WriteRecord     -> when pressed $ liftIO $ print "Trace not enabled at compilation"
#endif
                DelayUp         -> when pressed $ doDelayUp
                DelayDown       -> when pressed $ doDelayDown
                DelayLeft       -> when pressed $ liftIO $ print "Left"
                DelayRight      -> when pressed $ liftIO $ print "Right"

