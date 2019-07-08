module ALU where

import AcornAtom
import Data.Bits hiding (bit)
import Data.Word
import Prelude hiding (last, and)
import Emulation
import Control.Lens
import Control.Monad
import CPU

-- {-# INLINABLE adc #-}
adc :: MonadAcorn Word8 -> MonadAcorn ()
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
sbc :: MonadAcorn Word8 -> MonadAcorn ()
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
cmp :: MonadAcorn Word8 -> MonadAcorn ()
cmp mode = do
    src <- mode
    oldA <- getA
    let new = i16 oldA-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

-- {-# INLINABLE asl #-}
asl :: ((Word8 -> MonadAcorn Word8) -> MonadAcorn ()) -> MonadAcorn ()
asl mode = mode $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setNZ new

-- {-# INLINABLE rol #-}
rol :: ((Word8 -> MonadAcorn Word8) -> MonadAcorn ()) -> MonadAcorn ()
rol mode = mode $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setNZ new

-- {-# INLINABLE lsr #-}
lsr :: ((Word8 -> MonadAcorn Word8) -> MonadAcorn ()) -> MonadAcorn ()
lsr mode = mode $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

-- {-# INLINABLE ror #-}
ror :: ((Word8 -> MonadAcorn Word8) -> MonadAcorn ()) -> MonadAcorn ()
ror mode = mode $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setNZ new

-- {-# INLINABLE dec #-}
dec :: ((Word8 -> MonadAcorn Word8) -> MonadAcorn ()) -> MonadAcorn ()
dec mode = mode $ \src -> setNZ (src-1)

-- {-# INLINABLE inc #-}
inc :: ((Word8 -> MonadAcorn Word8) -> MonadAcorn ()) -> MonadAcorn ()
inc mode = mode $ \src -> setNZ (src+1)

-- {-# INLINABLE bit #-}
bit :: MonadAcorn Word8 -> MonadAcorn ()
bit mode = do
    src <- mode
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src

-- {-# INLINABLE cpx #-}
cpx :: MonadAcorn Word8 -> MonadAcorn ()
cpx mode = do
    src <- mode
    rx <- getX
    let new = i16 rx-i16 src
    discard $ setNZ $ i8 new
    putC $ new < 0x100

-- {-# INLINABLE cpy #-}
cpy :: MonadAcorn Word8 -> MonadAcorn ()
cpy mode = do
    src <- mode
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

-- 2 clock cycles
-- {-# INLINABLE txs #-}
txs :: MonadAcorn ()
txs = do
    tick 1
    discard $ getPC >>= readMemory
    getX >>= putS

-- 2 clock cycles
-- {-# INLINABLE tra #-}
tra :: MonadAcorn Word8 -> (Word8 -> MonadAcorn ()) ->
       MonadAcorn ()
tra getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    getReg >>= setNZ >>= putReg

-- 2 clock cycles
-- {-# INLINABLE inr #-}
inr :: MonadAcorn Word8 -> (Word8 -> MonadAcorn ()) -> MonadAcorn ()
inr getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
--     v0 <- getReg
--     let v1 = v0+1
--     discard $ setNZ v1
--     putReg v1
--     getReg >>= return . (+ 1) >>= setNZ >>= putReg
    getReg & fmap (+ 1) >>= setNZ >>= putReg

-- 2 clock cycles
-- {-# INLINABLE der #-}
der :: MonadAcorn Word8 -> (Word8 -> MonadAcorn ()) -> MonadAcorn ()
der getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
--     v0 <- getReg
--     let v1 = v0-1
--     discard $ setNZ v1
--     putReg v1
--     getReg >>= return . (subtract 1) >>= setNZ >>= putReg
    getReg & fmap (subtract 1) >>= setNZ >>= putReg

-- {-# INLINABLE ora #-}
ora :: MonadAcorn Word8 -> MonadAcorn ()
ora mode = do
    src <- mode
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setNZ_ newA
--     getA & fmap (.|. src) 

-- {-# INLINABLE and #-}
and :: MonadAcorn Word8 -> MonadAcorn ()
and mode = do
    src <- mode
    getA >>= setNZ . (src .&.) >>= putA

-- {-# INLINABLE eor #-}
eor :: MonadAcorn Word8 -> MonadAcorn ()
eor mode = do
    src <- mode
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    void $ setNZ newA

-- {-# INLINABLE lda #-}
lda :: MonadAcorn Word8 -> MonadAcorn ()
lda mode = mode >>= setNZ >>= putA

-- {-# INLINABLE sta #-}
sta :: (Word8 -> MonadAcorn()) -> MonadAcorn ()
sta mode = getA >>= mode

-- {-# INLINABLE stx #-}
stx :: (Word8 -> MonadAcorn()) -> MonadAcorn ()
stx mode = getX >>= mode

-- {-# INLINABLE ldx #-}
ldx :: MonadAcorn Word8 -> MonadAcorn ()
ldx mode = do
    src <- mode
    putX src
    setNZ_ src

-- {-# INLINABLE sty #-}
sty :: (Word8 -> MonadAcorn ()) -> MonadAcorn ()
sty mode = getY >>= mode

-- {-# INLINABLE ldy #-}
ldy :: MonadAcorn Word8 -> MonadAcorn ()
ldy mode = mode >>= setNZ >>= putY

