module Stella where

import Asm hiding (a)
import Atari2600
-- import Data.Bits hiding (bit)
import Data.Word
import Prelude hiding (last, and)
import Control.Lens
import Data.Bits.Lens
import System.Clock
import Control.Monad
import Data.Bits hiding (bit)
import BitManips
import Numeric
import Data.Int
import VideoOps
import Metrics
import Control.Monad.IO.Class
import Data.IORef

-- {- INLINE stellaHmclr -}
stellaHmclr :: MonadAtari ()
stellaHmclr =
    mapM_ (@= 0) [hmp0, hmp1, hmm0, hmm1, hmbl]

-- {- INLINE stellaCxclr -}
stellaCxclr :: MonadAtari ()
stellaCxclr =
    mapM_ (@= 0) [cxm0p, cxm1p, cxm0fb, cxm1fb, cxp0fb, cxp1fb, cxblpf, cxppmm]

-- {- INLINE stellaHmove -}
stellaHmove :: MonadAtari ()
stellaHmove = do
    pendingHmove @= True

    poffset0 <- load hmp0
    modify ppos0 $ \ppos0' ->  wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- load hmp1
    modify ppos1 $ \ppos1' ->  wrap160 (ppos1'-clockMove poffset1)

    moffset0 <- load hmm0
    modify mpos0 $ \mpos0' ->  wrap160 (mpos0'-clockMove moffset0)

    moffset1 <- load hmm1
    modify mpos1 $ \mpos1' ->  wrap160 (mpos1'-clockMove moffset1)

    boffset <- load hmbl
    modify bpos $ \bpos' -> wrap160 (bpos'-clockMove boffset)

-- {-# INLINE wrap160 #-}
wrap160 :: Int -> Int
wrap160 i | i < picx = wrap160 (i+160)
          | i >= picx+160 = wrap160 (i-160)
wrap160 i = i

-- {- INLINE stellaVblank -}
stellaVblank :: Word8 -> MonadAtari ()
stellaVblank v = do
    trigger1' <- load trigger1
    modify inpt4 $ bitAt 7 .~ not trigger1'
    trigger2' <- load trigger2
    modify inpt5 $ bitAt 7 .~ not trigger2'
    vblank @= v

-- {- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- load hpos
    -- Run instructions until we're at start of new scan line
    when (hpos' > 0) $ do
        stellaTickFor 1 -- there's a smarter way to do this XXX
        stellaWsync

stellaTickFor :: Int -> MonadAtari ()
stellaTickFor d = do
    n <- load ahead
    if d > n
        then do
            stellaTickFor' (d-n)
            ahead @= 0
        else ahead @= (n-d)

stellaTickFor' :: Int -> MonadAtari ()
stellaTickFor' diff = do
    when (diff >= 0) $ do
        -- Batch together items that don't need to be
        -- carried out on individual ticks
        modifyStellaClock id (+ fromIntegral diff)
        replicateM_ (fromIntegral diff) $ timerTick
        resmp0' <- load resmp0
        resmp1' <- load resmp1
        -- XXX surely this must be done every time - collisions
        clampMissiles resmp0' resmp1'

        parityRef <- view frameParity
        parity <- liftIO $ readIORef parityRef
        ptr' <- view (if parity then textureData else lastTextureData)
        -- XXX Not sure stellaDebug actually changes here so may be some redundancy
        stellaTick (fromIntegral diff) ptr'

timerTick' :: Word8 -> Int -> Int -> Word8 -> (Word8, Int, Int, Word8)
timerTick' 0      0         _         _       = (0xff,       3*1-1,         1,         0x80)
timerTick' intim' 0         interval' timint' = (intim'-1, 3*interval'-1, interval', timint')
timerTick' intim' subtimer' interval' timint' = (intim',   subtimer'-1,   interval', timint')

timerTick :: MonadAtari ()
timerTick = do
    (intim'', subtimer'', interval'', timint'') <- timerTick' <$> load intim <*> load subtimer <*> load interval <*> load timint
    intim @= intim''
    subtimer @= subtimer''
    interval @= interval''
    timint @= timint''

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
resetNextFrame :: MonadAtari ()
resetNextFrame = do
    liftIO $ print "Resetting clock"
    t <- liftIO $ getTime Realtime
    let nt = addTime t (1000000000 `div` fps)
    nextFrameTimeRef <- view nextFrameTime
    liftIO $ writeIORef nextFrameTimeRef nt

makePlayfield :: MonadAtari ()
makePlayfield = do
    pf0' <- load pf0
    pf1' <- load pf1
    pf2' <- load pf2
    ctrlpf' <- load ctrlpf
    let pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    pf @= pf'

startIntervalTimerN :: Int -> Word8 -> MonadAtari ()
startIntervalTimerN n v = do
    interval @= n
    subtimer @= 0 -- Was 3*n-1
    intim @= v
    timint @= 0

-- {- INLINABLE readStella -}
readStella :: Word16 -> MonadAtari Word8
readStella addr = do
    controls <- view controllers
    case addr of
        0x00 -> load cxm0p
        0x01 -> load cxm1p
        0x02 -> load cxp0fb
        0x03 -> load cxp1fb
        0x04 -> load cxm0fb
        0x05 -> load cxm1fb
        0x06 -> load cxblpf
        0x07 -> load cxppmm
        0x08 -> readInput controls inpt0 2
        0x09 -> readInput controls inpt1 1
        0x0a -> readInput controls inpt2 5
        0x0b -> readInput controls inpt3 4
        0x0c -> readInput controls inpt4 0
        0x0d -> readInput controls inpt5 3
        0x0e -> liftIO $ do
                    putStrLn "Illegal read 0xe"
                    return 0xe
        0x0f -> liftIO $ do
                    putStrLn "Illegal read 0xf"
                    return 0xf -- Hack for Haunted House
        0x10 -> load cxm0p
        0x11 -> load cxm1p
        0x12 -> load cxp0fb
        0x13 -> load cxp1fb
        0x14 -> load cxm0fb
        0x15 -> load cxm1fb
        0x16 -> load cxblpf
        0x17 -> load cxppmm
        0x1c -> load inpt4
        0x1d -> load inpt5
        0x20 -> load cxm0p
        0x21 -> load cxm1p
        0x22 -> load cxp0fb
        0x23 -> load cxp1fb
        0x24 -> load cxm0fb
        0x25 -> load cxm1fb
        0x26 -> load cxblpf
        0x27 -> load cxppmm
        0x2c -> load inpt4
        0x2d -> load inpt5
        0x30 -> load cxm0p
        0x31 -> load cxm1p
        0x32 -> load cxp0fb
        0x33 -> load cxp1fb
        0x34 -> load cxm0fb
        0x35 -> load cxm1fb
        0x36 -> load cxblpf
        0x37 -> load cxppmm
        0x3c -> load inpt4
        0x3d -> load inpt5
        0x280 -> load swcha
        0x282 -> load swchb
        0x284 -> load intim
        0x285 -> load timint
        _ -> return 0x00
--         a -> liftIO $ do
--                     putStrLn $ "Illegal read 0x" ++ showHex a ""
--                     return 0x0

graphicsDelay :: Int -> MonadAtari ()
graphicsDelay d = do
    n <- load ahead
    when (d > n) $ do
            stellaTickFor' (d-n)
            ahead @= d
