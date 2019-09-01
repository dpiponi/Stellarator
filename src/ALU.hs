module ALU where

import Atari2600
import Data.Bits hiding (bit)
import Data.Word
import Prelude hiding (last, and)
import Emulation
import Control.Lens
import Control.Monad
import CPU

-- {-# INLINABLE adc #-}
adc :: MonadAtari Word8 -> MonadAtari ()
adc mode = do
    src <- mode
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA+fromIntegral src+if carry then 1 else 0 :: Word16
    decimal <- getD
    setZ (i8 newA)
    if decimal
        then do
            let adjustedA = if (oldA .&. 0xf) + (src .&. 0xf) + (if carry then 1 else 0) > 9
                                then newA+6
                                else newA
            setN (i8 adjustedA)
            putV $ (complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` adjustedA) .&. 0x80) /= 0
            let readjustedA = if adjustedA > 0x99 then adjustedA+96 else adjustedA
            putC $ readjustedA > 0xff
            putA $ fromIntegral (readjustedA .&. 0xff)
        else do
            setN (i8 newA)
            putV $ (complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` newA) .&. 0x80) /= 0
            putC $ newA > 0xff
            putA $ fromIntegral (newA .&. 0xff)

-- {-# INLINABLE sbc #-}
sbc :: MonadAtari Word8 -> MonadAtari ()
sbc mode = do
    src <- mode
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    discard $ setNZ $ i8 newA
    putV $ (((i16 oldA `xor` i16 src) .&. 0x80) /= 0) && (((i16 oldA `xor` newA) .&. 0x80) /= 0)
    decimal <- getD
    if decimal
        then do
            let adjustedA = if iz (oldA .&. 0xf)-(if carry then 0 else 1) < iz (src .&. 0xf)
                                then newA - 6
                                else newA
            let readjustedA = if adjustedA > 0x99
                                then adjustedA-0x60
                                else adjustedA
            putA $ fromIntegral (readjustedA .&. 0xff)
            putC $ readjustedA < 0x100
        else do
            putA $ fromIntegral (newA .&. 0xff)
            putC $ newA < 0x100

-- {-# INLINABLE cmp #-}
cmp :: MonadAtari Word8 -> MonadAtari ()
cmp mode = do
    src <- mode
    oldA <- getA
    let new = i16 oldA-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

-- {-# INLINABLE asl #-}
asl :: ((Word8 -> MonadAtari Word8) -> MonadAtari ()) -> MonadAtari ()
asl mode = mode $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setNZ new

-- {-# INLINABLE rol #-}
rol :: ((Word8 -> MonadAtari Word8) -> MonadAtari ()) -> MonadAtari ()
rol mode = mode $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setNZ new

-- {-# INLINABLE lsr #-}
lsr :: ((Word8 -> MonadAtari Word8) -> MonadAtari ()) -> MonadAtari ()
lsr mode = mode $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

-- {-# INLINABLE ror #-}
ror :: ((Word8 -> MonadAtari Word8) -> MonadAtari ()) -> MonadAtari ()
ror mode = mode $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setNZ new

-- {-# INLINABLE dec #-}
dec :: ((Word8 -> MonadAtari Word8) -> MonadAtari ()) -> MonadAtari ()
dec mode = mode $ \src -> setNZ (src-1)

-- {-# INLINABLE inc #-}
inc :: ((Word8 -> MonadAtari Word8) -> MonadAtari ()) -> MonadAtari ()
inc mode = mode $ \src -> setNZ (src+1)

-- {-# INLINABLE bit #-}
bit :: MonadAtari Word8 -> MonadAtari ()
bit mode = do
    src <- mode
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src

-- {-# INLINABLE cpx #-}
cpx :: MonadAtari Word8 -> MonadAtari ()
cpx mode = do
    src <- mode
    rx <- getX
    let new = i16 rx-i16 src
    discard $ setNZ $ i8 new
    putC $ new < 0x100

-- {-# INLINABLE cpy #-}
cpy :: MonadAtari Word8 -> MonadAtari ()
cpy mode = do
    src <- mode
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

-- 2 clock cycles
-- {-# INLINABLE txs #-}
txs :: MonadAtari ()
txs = do
    discard $ getPC >>= readMemoryTick
    getX >>= putS

-- 2 clock cycles
-- {-# INLINABLE tra #-}
tra :: MonadAtari Word8 -> (Word8 -> MonadAtari ()) ->
       MonadAtari ()
tra getReg putReg = do
    discard $ getPC >>= readMemoryTick
    getReg >>= setNZ >>= putReg

-- 2 clock cycles
-- {-# INLINABLE inr #-}
inr :: MonadAtari Word8 -> (Word8 -> MonadAtari ()) -> MonadAtari ()
inr getReg putReg = do
    discard $ getPC >>= readMemoryTick
    getReg & fmap (+ 1) >>= setNZ >>= putReg

-- 2 clock cycles
-- {-# INLINABLE der #-}
der :: MonadAtari Word8 -> (Word8 -> MonadAtari ()) -> MonadAtari ()
der getReg putReg = do
    discard $ getPC >>= readMemoryTick
    getReg & fmap (subtract 1) >>= setNZ >>= putReg

-- {-# INLINABLE ora #-}
ora :: MonadAtari Word8 -> MonadAtari ()
ora mode = do
    src <- mode
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setNZ_ newA
--     getA & fmap (.|. src) 

-- {-# INLINABLE and #-}
and :: MonadAtari Word8 -> MonadAtari ()
and mode = do
    src <- mode
    getA >>= setNZ . (src .&.) >>= putA

-- {-# INLINABLE eor #-}
eor :: MonadAtari Word8 -> MonadAtari ()
eor mode = do
    src <- mode
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    void $ setNZ newA

-- {-# INLINABLE lda #-}
lda :: MonadAtari Word8 -> MonadAtari ()
lda mode = mode >>= setNZ >>= putA

-- {-# INLINABLE sta #-}
sta :: (Word8 -> MonadAtari()) -> MonadAtari ()
sta mode = getA >>= mode

-- {-# INLINABLE stx #-}
stx :: (Word8 -> MonadAtari()) -> MonadAtari ()
stx mode = getX >>= mode

-- {-# INLINABLE ldx #-}
ldx :: MonadAtari Word8 -> MonadAtari ()
ldx mode = do
    src <- mode
    putX src
    setNZ_ src

-- {-# INLINABLE sty #-}
sty :: (Word8 -> MonadAtari ()) -> MonadAtari ()
sty mode = getY >>= mode

-- {-# INLINABLE ldy #-}
ldy :: MonadAtari Word8 -> MonadAtari ()
ldy mode = mode >>= setNZ >>= putY

