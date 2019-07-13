module Stella where

import AcornAtom
import Asm hiding (a)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.IORef
import Data.Int
import Data.Word
import Metrics
import Prelude hiding (last, and)
import System.Clock

-- stellaTickFor :: Int -> MonadAcorn ()
-- stellaTickFor d = do
--     n <- load ahead
--     if d > n
--         then do
--             stellaTickFor' (d-n)
--         else return ()
-- 
-- stellaTickFor' :: Int -> MonadAcorn ()
-- stellaTickFor' diff = do
--     when (diff >= 0) $ do
--         modifyStellaClock id (+ fromIntegral diff)

-- add nanoseconds
addTime :: TimeSpec -> Int64 -> TimeSpec
addTime (TimeSpec a b) c =
    let d = b + c
    in if d >= 1000000000 then TimeSpec (a+1) (d-1000000000) else TimeSpec a d

gtTime :: TimeSpec -> TimeSpec -> Bool
gtTime (TimeSpec sec0 nsec0) (TimeSpec sec1 nsec1) =
    sec0 > sec1 || sec0 == sec1 && nsec0 > nsec1

-- If the emulator is just starting, or restarting after a pause,
-- then the time for the next frame needs to be pushed forward
-- until after the current time.
resetNextFrame :: MonadAcorn ()
resetNextFrame = do
    liftIO $ print "Resetting clock"
    t <- liftIO $ getTime Realtime
    let nt = addTime t (1000000000 `div` fps)
    nextFrameTimeRef <- view nextFrameTime
    liftIO $ writeIORef nextFrameTimeRef nt
