module Events where

import SDL.Event
import SDL.Input.Keyboard
import Keys
import Atari2600
import SDL.Vect
import Emulation
import qualified SDL
import Control.Monad.Reader
import Control.Lens
import Asm
import Data.Bits
import Data.Bits.Lens
import System.Exit
import Control.Concurrent
import Debugger

{- INLINE isPressed -}
isPressed :: InputMotion -> Bool
isPressed Pressed = True
isPressed Released = False

handleEvent :: [(Scancode, AtariKey)] -> EventPayload -> MonadAtari ()

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
