module Events where

-- import SDL.Event
-- import SDL.Input.Keyboard
import Keys hiding (debugMode)
import AcornAtom
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

atom_keyboard :: [(Key, ([Int], Int))]
atom_keyboard = [
    (Key'LeftShift, ([0..9], 7)),
    (Key'RightShift, ([0..9], 7)),
    (Key'LeftControl, ([0..9], 6)),

    (Key'Escape, ([0], 5)),
    (Key'Q, ([0], 4)),
    (Key'G, ([0], 3)),
    (Key'Minus, ([0], 2)),
    (Key'3, ([0], 1)),

    (Key'Z, ([1], 5)),
    (Key'P, ([1], 4)),
    (Key'F, ([1], 3)),
    (Key'Comma, ([1], 2)),
    (Key'2, ([1], 1)),

    (Key'Y, ([2], 5)),
    (Key'O, ([2], 4)),
    (Key'E, ([2], 3)),
    (Key'Semicolon, ([2], 2)),
    (Key'1, ([2], 1)),
    (Key'Up, ([2], 0)),
    (Key'Down, ([2], 0)),

    (Key'X, ([3], 5)),
    (Key'N, ([3], 4)),
    (Key'D, ([3], 3)),
    (Key'Equal, ([3], 2)),
    (Key'0, ([3], 1)),
    (Key'Left, ([3], 0)),
    (Key'Right, ([3], 0)),

    (Key'W, ([4], 5)),
    (Key'M, ([4], 4)),
    (Key'C, ([4], 3)),
    (Key'9, ([4], 2)),
    (Key'Backspace, ([4], 1)),
    (Key'CapsLock, ([4], 0)),

    (Key'V, ([5], 5)),
    (Key'L, ([5], 4)),
    (Key'B, ([5], 3)),
    (Key'8, ([5], 2)),
    (Key'PageDown, ([5], 1)),
    (Key'PageUp, ([5], 0)),

    (Key'U, ([6], 5)),
    (Key'K, ([6], 4)),
    (Key'A, ([6], 3)),
    (Key'7, ([6], 2)),
    (Key'Enter, ([6], 1)),
    (Key'RightBracket, ([6], 0)),

    (Key'T, ([7], 5)),
    (Key'J, ([7], 4)),
    (Key'Apostrophe, ([7], 3)),
    (Key'6, ([7], 2)),
    (Key'Backslash, ([7], 0)),

    (Key'S, ([8], 5)),
    (Key'I, ([8], 4)),
    (Key'Slash, ([8], 3)),
    (Key'5, ([8], 2)),
    (Key'LeftBracket, ([8], 0)),

    (Key'R, ([9], 5)),
    (Key'H, ([9], 4)),
    (Key'Period, ([9], 3)),
    (Key'4, ([9], 2)),
    (Key'Space, ([9], 0))]

updatePPIA :: Key -> Bool -> MonadAcorn ()
updatePPIA key pressed = do
    let op = lookup key atom_keyboard
    case op of
        Nothing -> return ()
        Just (rows, column) -> do
            forM_ rows $ \row -> do
                modify (keyboard_matrix + TO row) $ bitAt column .~ not pressed
--                 liftIO $ print (row, column, pressed)
                

handleKey :: KeyState -> Key -> MonadAcorn ()
handleKey motion key = do
--     let scancode = keysymScancode sym
--     let mAtariKey = M.lookup key atariKeys
    let pressed = isPressed motion
    case key of
--                 DumpState        -> Emulation.dumpState
                Key'GraveAccent         -> liftIO $ exitSuccess
                Key'LeftAlt    -> when pressed $ do
                                        -- Throw away SDL events
                                        -- Rewrite as a withXXX XXX
--                                         t <- liftIO $ forkIO $ let spin = SDL.pollEvents >> spin in spin
                                        Emulation.dumpState
                                        runDebugger
--                                         liftIO $ killThread t
                                        resetNextFrame
                key -> updatePPIA key pressed
