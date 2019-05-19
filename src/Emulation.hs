{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

-- module Emulation(stellaDebug,
--                  dumpRegisters,
--                  dumpState,
--                  clock,
--                  stellaClock,
--                  initState,
--                  trigger1,
--                  trigger2,
--                  loopUntil,
--                  initHardware,
--                  load,
--                  getA) where

module Emulation where

import Asm()
import System.Clock
import Graphics.Rendering.OpenGL as GL
import Atari2600
import BitManips
import Display
import Control.Lens hiding (set)
import Control.Concurrent
import Control.Monad.Reader
import System.Clock
import Data.Array.IO
import Data.Bits hiding (bit)
import Data.Bits.Lens
#if TRACE
import Data.Array.Storable
#endif
import Data.IORef
import Data.Int
import DebugState
import Disasm hiding (make16)
import Foreign.Ptr
import Memory
import Metrics
import Numeric
import Prelude hiding (last, and)
import Asm
import VideoOps hiding (bit)
import qualified SDL
import Data.Word
import Data.Bits hiding (bit)
import Numeric

readMemory :: Word16 -> MonadAtari Word8
writeMemory :: Word16 -> Word8 -> MonadAtari ()
illegal :: Word8 -> MonadAtari ()

--
-- {-# INLINE readMemory #-}
readMemory addr' = do
    let addr = addr' .&. 0x1fff -- 6507
    byte <- pureReadMemory (memoryType addr) addr

    atari <- ask
    let bankStateRef = atari ^. bankState
    liftIO $ modifyIORef bankStateRef $ bankSwitch addr 0

    return byte

-- {-# INLINE writeMemory #-}
writeMemory addr' v = do
    let addr = addr' .&. 0x1fff -- 6507
    pureWriteMemory (memoryType addr) addr v

    atari <- ask
    let bankStateRef = atari ^. bankState
    liftIO $ modifyIORef bankStateRef $ bankSwitch addr v

-- {-# INLINE tick #-}
tick :: Int -> MonadAtari ()
tick n = do
    modifyClock id (+ fromIntegral n)
    -- c <- useClock id
    stellaTickFor (3*n)

-- {-# INLINE debugStr #-}
debugStr _ _ = return ()
-- {-# INLINE debugStrLn #-}
debugStrLn _ _ = return ()

-- {- INLINE illegal -}
illegal i = do
    dumpState
    error $ "Illegal opcode 0x" ++ showHex i ""

debugStr :: Int -> String -> MonadAtari ()
debugStrLn :: Int -> String -> MonadAtari ()

-- {-# INLINE make16 #-}
make16 :: Word8 -> Word8 -> Word16
make16 lo0 hi0 = (i16 hi0 `shift` 8)+i16 lo0

-- {-# INLINE incPC #-}
incPC :: MonadAtari ()
incPC = addPC 1

-- {-# INLINABLE read16 #-}
read16 :: Word16 -> MonadAtari Word16
read16 addr = do
    lo0 <- readMemory addr
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

-- {-# INLINABLE read16tick #-}
read16tick :: Word16 -> MonadAtari Word16
read16tick addr = do
    tick 1
    lo0 <- readMemory addr
    tick 1
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

-- {-# INLINABLE read16zpTick #-}
read16zpTick :: Word8 -> MonadAtari Word16
read16zpTick addr = do
    tick 1
    lo0 <- readMemory (i16 addr)
    tick 1
    hi0 <- readMemory (i16 addr+1)
    return $ make16 lo0 hi0

-- http://www.emulator101.com/6502-addressing-modes.html

-- {-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

-- {-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

-- {-# INLINE iz #-}
iz :: Integral a => a -> Int
iz = fromIntegral

-- Note, a 6502 performs a read or write *every* clock cycle
-- regardless of what instruction is being executed.

-- 6 clock cycles...
-- {-# INLINABLE writeIndX #-}
writeIndX :: Word8 -> MonadAtari ()
writeIndX src = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    addrX <- read16zpTick (addr+index)

    tick 1
    writeMemory addrX src
    incPC

-- 3 clock cycles
-- {-# INLINABLE writeZeroPage #-}
writeZeroPage :: Word8 -> MonadAtari ()
writeZeroPage src = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    writeMemory (i16 addr) src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeAbs #-}
writeAbs :: Word8 -> MonadAtari()
writeAbs src = do
    addr <- getPC >>= read16tick

    tick 1
    writeMemory addr src
    addPC 2

-- 6 clock cycles
-- {-# INLINABLE writeIndY #-}
writeIndY :: Word8 -> MonadAtari ()
writeIndY src = do
    tick 1
    index <- getY
    addr' <- getPC >>= readMemory

    addr <- read16zpTick addr'

    let (halfAddrY, addrY) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeZeroPageX #-}
writeZeroPageX :: Word8 -> MonadAtari ()
writeZeroPageX src = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    writeMemory (i16 $ addr+index) src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeZeroPageY #-}
writeZeroPageY :: Word8 -> MonadAtari ()
writeZeroPageY src = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    writeMemory (i16 $ addr+index) src
    incPC

-- 5 clock cycles
-- {-# INLINABLE writeAbsY #-}
writeAbsY :: Word8 -> MonadAtari ()
writeAbsY src = do
    index <- getY
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrY, addrY) = halfSum addr index
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    addPC 2

-- 5 clock cycles
-- {-# INLINABLE writeAbsX #-}
writeAbsX :: Word8 -> MonadAtari ()
writeAbsX src = do
    index <- getX
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrX, addrX) = halfSum addr index
    discard $ readMemory halfAddrX

    tick 1
    writeMemory addrX src
    addPC 2

-- 6 clock cycles
-- {-# INLINABLE readIndX #-}
readIndX :: MonadAtari Word8
readIndX = do
    tick 1
    index <- getX
    addr0 <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr0)

    addr1 <- read16zpTick (addr0+index)

    tick 1
    incPC
    readMemory addr1

-- 3 clock cycles
-- {-# INLINABLE readZeroPage #-}
readZeroPage :: MonadAtari Word8
readZeroPage = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)
    incPC
    return src

-- 2 clock cycles
-- {-# INLINABLE readImm #-}
readImm :: MonadAtari Word8
readImm = do
    tick 1
    src <- getPC >>= readMemory
    incPC
    return src

-- XXX consider applicable ops like *>
-- 4 clock cycles
-- {-# INLINABLE readAbs #-}
readAbs :: MonadAtari Word8
readAbs = do
    p0 <- getPC
    src <- (read16tick p0 <* tick 1) >>= readMemory
    addPC 2
    return src

-- 5-6 clock cycles
-- {-# INLINABLE readIndY #-}
readIndY :: MonadAtari Word8
readIndY = do
    tick 1
    addr' <- getPC >>= readMemory

    addr <- read16zpTick addr'

    index <- getY
    let (halfAddrY, addrY) = halfSum addr index

    when (halfAddrY /= addrY) $ do
        tick 1
        discard $ readMemory halfAddrY

    tick 1
    src <- readMemory addrY
    incPC
    return src

-- 4 clock cycles
-- {-# INLINABLE readZeroPageX #-}
readZeroPageX :: MonadAtari Word8
readZeroPageX = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    incPC
    readMemory (i16 $ addr+index)

-- 4 clock cycles
-- {-# INLINABLE readZeroPageY #-}
readZeroPageY :: MonadAtari Word8
readZeroPageY = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    incPC
    readMemory (i16 $ addr+index)

{-# inline halfSum #-}
halfSum :: Word16 -> Word8 -> (Word16, Word16)
halfSum addr index = 
    let fullSum = addr+i16 index
    in (make16 (lo addr+index) (hi addr), fullSum)

-- {-# INLINABLE halfSignedSum #-}
halfSignedSum :: Word16 -> Word8 -> (Word16, Word16)
halfSignedSum addr index = 
    let fullSum = if index < 0x80 then addr+i16 index else addr+i16 index-0x100
    in (make16 (lo addr+index) (hi addr), fullSum)

-- 4-5 clock cycles
-- {-# INLINABLE readAbsX #-}
readAbsX :: MonadAtari Word8
readAbsX = do
    index <- getX
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrX, addrX) = halfSum addr index
    when (halfAddrX /= addrX) $ do
            tick 1
            discard $ readMemory halfAddrX

    tick 1
    readMemory addrX

-- 4-5 clock cycles
-- {-# INLINABLE readAbsY #-}
readAbsY :: MonadAtari Word8
readAbsY = do
    index <- getY
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrY, addrY) = halfSum addr index
    when ( halfAddrY /= addrY) $ do
            tick 1
            discard $ readMemory halfAddrY

    tick 1
    readMemory addrY

-- 2-4 clock cycles
-- {-# INLINABLE bra #-}
bra :: MonadAtari Bool -> Bool -> MonadAtari ()
bra getFlag value = do
    tick 1
    offset <- getPC >>= readMemory
    f <- getFlag
    incPC

    when (value == f) $ do
        tick 1
        discard $ getPC >>= readMemory

        oldP <- getPC
        let (halfAddr, addr) = halfSignedSum oldP offset
        when (halfAddr /= addr) $ do
                tick 1
                discard $ readMemory halfAddr
        putPC addr

-- 2 clock cycles
-- {-# INLINABLE set #-}
set :: (Bool -> MonadAtari ()) -> Bool -> MonadAtari ()
set putFlag value = do
    tick 1
    discard $ getPC >>= readMemory
    putFlag value

-- 2 clock cycles
-- {-# INLINABLE nop #-}
nop :: MonadAtari ()
nop = do
    tick 1
    discard $ getPC >>= readMemory

{-
-- 3 clock cycles. Undocumented.
-- {-# INLINABLE nop #-}
dop :: MonadAtari ()
nop = do
    tick 1
    discard $ getPC >>= readMemory
-}

-- 3 clock cycles
-- {-# INLINABLE jmp #-}
jmp :: MonadAtari ()
jmp = getPC >>= read16tick >>= putPC

-- 5 clock cycles
-- NB address wraps around in page XXX
-- Not correct here.
-- Looks like the torture test might not catch this.
-- Aha! That's why ALIGN is used before addresses!
-- {-# INLINABLE jmp_indirect #-}
jmp_indirect :: MonadAtari ()
jmp_indirect = do
    getPC >>= read16tick >>= read16tick >>= putPC

-- {-# INLINABLE uselessly #-}
uselessly :: m () -> m ()
uselessly = id

-- 5 clock cycles
-- {-# INLINABLE withZeroPage #-}
withZeroPage :: (Word8 -> MonadAtari Word8) -> MonadAtari ()
withZeroPage op = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)

    tick 1
    uselessly $ writeMemory (i16 addr) src

    tick 1
    op src >>= writeMemory (i16 addr)
    incPC

-- 2 clock cycles
-- {-# INLINABLE withAcc #-}
withAcc :: (Word8 -> MonadAtari Word8) -> MonadAtari ()
withAcc op = do
    tick 1
    discard $ getPC >>= readMemory
    getA >>= op >>= putA

-- 6 clock cycles
-- {-# INLINE withAbs #-}
withAbs :: (Word8 -> MonadAtari Word8) -> MonadAtari ()
withAbs op = do
    addr <- getPC >>= read16tick
    
    tick 1
    src <- readMemory addr

    tick 1
    uselessly $ writeMemory addr src

    tick 1
    dst <- op src
    addPC 2
    writeMemory addr dst

-- 6 clock cycles
withZeroPageX :: (Word8 -> MonadAtari Word8) -> MonadAtari ()
withZeroPageX op = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory
    let addrX = addr+index

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    src <- readMemory (i16 addrX)

    tick 1
    writeMemory (i16 addrX) src

    tick 1
    dst <- op src
    writeMemory (i16 addrX) dst
    incPC
 
-- 7 clock cycles
-- {-# INLINE withAbsX #-}
withAbsX :: (Word8 -> MonadAtari Word8) -> MonadAtari ()
withAbsX op = do
    p0 <- getPC
    index <- getX
    addr <- read16tick p0

    let (halfAddrX, addrX) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrX

    tick 1
    src <- readMemory addrX

    tick 1
    uselessly $ writeMemory addrX src

    tick 1
    addPC 2
    dst <- op src
    writeMemory addrX dst

-- {-# INLINABLE setN #-}
setN :: Word8 -> MonadAtari ()
setN r = putN $ r >= 0x80

-- {-# INLINABLE setZ #-}
setZ :: Word8 -> MonadAtari ()
setZ r = putZ $ r == 0

-- {-# INLINABLE setNZ #-}
setNZ :: Word8 -> MonadAtari Word8
setNZ r = setN r >> setZ r >> return r

-- {-# INLINABLE setNZ_ #-}
setNZ_ :: Word8 -> MonadAtari ()
setNZ_ r = setN r >> setZ r

-- {-# INLINABLE ora #-}
ora :: MonadAtari Word8 -> MonadAtari ()
ora mode = do
    src <- mode
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setNZ_ newA

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

-- {-# INLINABLE stx #-}
stx :: (Word8 -> MonadAtari()) -> MonadAtari ()
stx mode = getX >>= mode

-- {-# INLINABLE ldx #-}
ldx :: MonadAtari Word8 -> MonadAtari ()
ldx mode = do
    src <- mode
    putX src
    setNZ_ src

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

-- {-# INLINABLE sty #-}
sty :: (Word8 -> MonadAtari ()) -> MonadAtari ()
sty mode = getY >>= mode

-- {-# INLINABLE ldy #-}
ldy :: MonadAtari Word8 -> MonadAtari ()
ldy mode = mode >>= setNZ >>= putY

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
    tick 1
    discard $ getPC >>= readMemory
    getX >>= putS

-- 2 clock cycles
-- {-# INLINABLE tra #-}
tra :: MonadAtari Word8 -> (Word8 -> MonadAtari ()) ->
       MonadAtari ()
tra getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    getReg >>= setNZ >>= putReg

-- 2 clock cycles
-- {-# INLINABLE inr #-}
inr :: MonadAtari Word8 -> (Word8 -> MonadAtari ()) -> MonadAtari ()
inr getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    v0 <- getReg
    let v1 = v0+1
    discard $ setNZ v1
    putReg v1

-- 2 clock cycles
-- {-# INLINABLE der #-}
der :: MonadAtari Word8 -> (Word8 -> MonadAtari ()) -> MonadAtari ()
der getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    v0 <- getReg
    let v1 = v0-1
    discard $ setNZ v1
    putReg v1

discard :: MonadAtari Word8 -> MonadAtari ()
discard = void

-- 7 clock cycles
-- {-# INLINABLE brk #-}
brk :: MonadAtari ()
brk = do
    tick 1
    p0 <- getPC
    incPC
    discard $ readMemory p0

    p1 <- getPC
    incPC
    tick 1
    push $ hi p1

    incPC
    tick 1
    push $ lo p1

    putB True
    incPC
    tick 1
    getP >>= push . (.|. 0x20) -- always on bit
    putI True

    read16tick 0xfffe >>= putPC -- irq/brk XXX

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
-- XXX not supported correctly for now
-- {-# INLINABLE irq #-}
irq :: MonadAtari ()
irq = do
    fi <- getI
    if not fi
        then nmi False
        else return ()

-- {-# INLINABLE push #-}
push :: Word8 -> MonadAtari ()
push v = do
    sp <- getS
    writeMemory (0x100+i16 sp) v
    putS (sp-1)

-- {-# INLINABLE pull #-}
pull :: MonadAtari Word8
pull = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemory (0x100+i16 sp')

-- 3 clock cycles
-- {-# INLINABLE pha #-}
pha :: MonadAtari ()
pha = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getA >>= push

-- 3 clock cycles
-- {-# INLINABLE php #-}
php :: MonadAtari ()
php = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getP >>= push . (.|. 0x30)

-- 4 clock cycles
-- {-# INLINABLE plp #-}
plp :: MonadAtari ()
plp = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= putP

-- 4 clock cycles
-- {-# INLINABLE pla #-}
pla :: MonadAtari ()
pla = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= setNZ >>= putA

-- {-# INLINABLE lo #-}
lo :: Word16 -> Word8
lo = i8

-- {-# INLINABLE hi #-}
hi :: Word16 -> Word8
hi a = i8 (a `shift` (-8))

-- {-# INLINABLE nmi #-}
nmi :: Bool -> MonadAtari ()
nmi sw = do
    p0 <- getPC
    push $ hi p0
    push $ lo p0
    putB sw
    getP >>= push . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk XXX
    tick 7

-- 6 clock cycles
-- {-# INLINABLE rti #-}
rti :: MonadAtari ()
rti = do
    tick 1
    p0 <- getPC
    void $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100 + fromIntegral s)

    tick 1
    pull >>= putP

    make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull) >>= putPC

-- 6 clock cycles
-- {-# INLINABLE jsr #-}
jsr :: MonadAtari ()
jsr = do
    tick 1
    p0 <- getPC
    pcl <- readMemory p0
    incPC

    tick 1
    s <- getS
    discard $ readMemory (0x100 + fromIntegral s)

    p2 <- getPC

    tick 1
    push $ hi p2

    tick 1
    push $ lo p2

    tick 1
    pch <- readMemory p2

    putPC $ make16 pcl pch

-- 6 clock cycles
-- {-# INLINABLE rts #-}
rts :: MonadAtari ()
rts = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    p0 <- make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull)
    
    tick 1
    discard $ readMemory p0
    putPC (p0+1)

timerTick' :: Word8 -> Int -> Int -> Word8 -> (Word8, Int, Int, Word8)
timerTick' 0      0         _         _       = (-1,       3*1-1,         1,         0x80)
timerTick' intim' 0         interval' timint' = (intim'-1, 3*interval'-1, interval', timint')
timerTick' intim' subtimer' interval' timint' = (intim',   subtimer'-1,   interval', timint')

timerTick :: MonadAtari ()
timerTick = do
    (intim'', subtimer'', interval'', timint'') <- timerTick' <$> load intim <*> load subtimer <*> load interval <*> load timint
    intim @= intim''
    subtimer @= subtimer''
    interval @= interval''
    timint @= timint''

startIntervalTimerN :: Int -> Word8 -> MonadAtari ()
startIntervalTimerN n v = do
    interval @= n
    subtimer @= 0 -- Was 3*n-1
    intim @= v
    timint @= 0

makeDelayArray:: [(Word16, Int)] -> IO (IOUArray Word16 Int)
makeDelayArray delayList = do
    delayArray <- newArray (0, 0x2c) 0
    forM_ delayList $ \(addr, d) -> writeArray delayArray addr d
    return delayArray

initState :: Int -> Int -> Int -> Int ->
             IOUArray Int Word8 ->
#if TRACE
             StorableArray Int Word8 ->
#endif
             BankState ->
             IOUArray Int Word8 ->
             Word16 ->
             SDL.Window -> 
             GL.Program ->
             GL.AttribLocation ->
             GL.TextureObject ->
             GL.TextureObject ->
             Ptr Word8 ->
             Ptr Word8 ->
             [(Word16, Int)] ->
             Controllers ->
             IO Atari2600
initState xscale' yscale' width height ram'
#if TRACE
            record'
#endif
            initBankState rom' initialPC window prog attrib initTex initLastTex initTextureData initLastTextureData delayList controllerType = do
          stellaDebug' <- newIORef DebugState.start
          bankState' <- newIORef initBankState
          t <- liftIO $ getTime Realtime
          let nt = addTime t (1000000000 `div` 60)
          nextFrameTime' <- newIORef nt
          parity <- newIORef False
          clock' <- newIORef 0
          -- debug' <- newIORef 8
          stellaClock' <- newIORef 0
#if TRACE
          recordPtr' <- newIORef 0
#endif
          boolArray' <- newArray (0, maxBool) False
          intArray' <- newArray (0, maxInt) 0      -- Overkill
          word64Array' <- newArray (0, maxWord64) 0
          word16Array' <- newArray (0, maxWord16) 0      -- Overkill
          word8Array' <- newArray (0, maxWord8) 0
          liftIO $ st word16Array' pc initialPC
          delayArray <- makeDelayArray delayList
          return $ Atari2600 {
              _frameParity = parity,
              _nextFrameTime = nextFrameTime',
              _xscale = xscale',
              _yscale = yscale',
              _windowWidth = width,
              _windowHeight = height,
              _rom = rom',
#if TRACE
              _record = record',
              _recordPtr = recordPtr',
#endif
              _ram = ram',
              _stellaDebug = stellaDebug',
              _bankState = bankState',
              _clock = clock',
              _stellaClock = stellaClock',
              _boolArray = boolArray',
              _intArray = intArray',
              _word64Array = word64Array',
              _word16Array = word16Array',
              _word8Array = word8Array',
              _controllers = controllerType,
              _sdlWindow = window,
              _textureData = initTextureData,
              _lastTextureData = initLastTextureData,
              _tex = initTex,
              _lastTex = initLastTex,
              _glProg = prog,
              _glAttrib = attrib,
              _delays = delayArray
          }

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

-- -- {-# INLINE iz #-}
-- iz :: Word16 -> Int -- or NUM
-- iz = fromIntegral

{-
Here's a standard kernel:
StartOfFrame
        ;--------------------------------------------------
        ; Start of vertical blank processing
        ;--------------------------------------------------
                lda #0
                sta VBLANK

                lda #2
                sta VSYNC

                sta WSYNC
                sta WSYNC
                sta WSYNC                ; 3 scanlines of VSYNC signal

                lda #0
                sta VSYNC
        ;--------------------------------------------------
        ; 37 scanlines of vertical blank...
        ;--------------------------------------------------
                ldx #0
VerticalBlank   sta WSYNC
                inx
                cpx #37
                bne VerticalBlank
        ;--------------------------------------------------
        ; Do 192 scanlines of colour-changing (our picture)
        ;--------------------------------------------------
                ldx #0                  ; this counts our scanline number
                ...
Lines           sta WSYNC
                inx
                cpx #192
                bne Lines
        ;--------------------------------------------------
        ; 30 scanlines of overscan...
        ;--------------------------------------------------
                lda #%01000010
                sta VBLANK           ; end of screen - enter blanking

                ldx #0
Overscan        sta WSYNC
                inx
                cpx #30
                bne Overscan

                jmp StartOfFrame
-}

-- {- INLINE stellaVblank -}
stellaVblank :: Word8 -> MonadAtari ()
stellaVblank v = do
    trigger1' <- load trigger1
    modify inpt4 $ bitAt 7 .~ not trigger1'
    trigger2' <- load trigger2
    modify inpt5 $ bitAt 7 .~ not trigger2'
    vblank @= v

makePlayfield :: MonadAtari ()
makePlayfield = do
    pf0' <- load pf0
    pf1' <- load pf1
    pf2' <- load pf2
    ctrlpf' <- load ctrlpf
    let pf' = assemblePlayfield (testBit ctrlpf' 0) pf0' pf1' pf2'
    pf @= pf'

-- Keyboard wiring
--
-- |key00 - D4 IN4|key01 - D4 IN1|key02 - D4 IN0|key03 - D4 IN5|key04 - D4 IN3|key05 - D4 IN2|
-- |key10 - D5 IN4|key11 - D5 IN1|key12 - D5 IN0|key13 - D5 IN5|key14 - D5 IN3|key15 - D5 IN2|
-- |key20 - D6 IN4|key21 - D6 IN1|key22 - D6 IN0|key23 - D6 IN5|key24 - D6 IN3|key25 - D6 IN2|
-- |key30 - D7 IN4|key31 - D7 IN1|key32 - D7 IN0|key33 - D7 IN5|key34 - D7 IN3|key35 - D7 IN2|

-- {- INLINABLE readStella -}
readStella :: Word16 -> MonadAtari Word8
readStella addr = do
--     liftIO $ putStrLn $ "reading 0x" ++ showHex addr ""
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
        _ -> return 0 

-- {- INLINE stellaVsync -}
stellaVsync :: Word8 -> MonadAtari ()
stellaVsync v = do
    oldv <- load vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
        vpos @= 0
        renderDisplay
    vsync @= v

-- {- INLINE stellaWsync -}
stellaWsync :: MonadAtari ()
stellaWsync = do
    hpos' <- load hpos
    -- Run instructions until we're at start of new scan line
    when (hpos' > 0) $ do
        stellaTickFor 1 -- there's a smarter way to do this XXX
        stellaWsync

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

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

-- {-# INLINE pureReadRom #-}
-- | pureReadRom sees address in full 6507 range 0x0000-0x1fff
pureReadRom :: Word16 -> MonadAtari Word8
pureReadRom addr = do
    atari <- ask
    let m = atari ^. rom
    let bankStateRef = atari ^. bankState
    bankState' <- liftIO $ readIORef bankStateRef
    let bankedAddress = bankAddress bankState' addr
    liftIO $ readArray m bankedAddress

-- {-# INLINE pureWriteRom #-}
-- | pureWriteRom sees address in full 6507 range 0x0000-0x1fff
-- You can write to Super Chip "ROM"
pureWriteRom :: Word16 -> Word8 -> MonadAtari ()
pureWriteRom addr v = do
    atari <- ask
    let m = atari ^. rom
    let bankStateRef = atari ^. bankState
    bankState' <- liftIO $ readIORef bankStateRef
    when (bankWritable bankState' addr) $ do
        let bankedAddress = bankAddress bankState' addr
        liftIO $ writeArray m bankedAddress v

-- {-# INLINE pureReadMemory #-}
-- | pureReadMemory expects an address in range 0x0000-0x1fff
-- The 'pure' refers to the fact that there are no side effects,
-- i.e. it won't trigger bank switching.
--
-- From http://atariage.com/forums/topic/27190-session-5-memory-architecture/
--
-- Atari 2600 Memory Map:
----------------------
-- $0000-002F TIA Primary Image
-- $0030-005F [shadow] TIA
-- $0060-007F [shadow-partial] TIA
-- $0080-00FF 128 bytes of RAM Primary Image (zero page image)
-- $0100-002F [shadow] TIA
-- $0130-005F [shadow] TIA
-- $0160-017F [shadow-partial] TIA
-- $0180-01FF [shadow] 128 bytes of RAM (CPU stack image)
-- $0200-022F [shadow] TIA
-- $0230-025F [shadow] TIA
-- $0260-027F [shadow-partial] TIA
-- $0280-029F 6532-PIA I/O ports and timer Primary image
-- $02A0-02BF [shadow] 6532-PIA
-- $02C0-02DF [shadow] 6532-PIA
-- $02D0-02FF [shadow] 6532-PIA
-- $0300-032F [shadow] TIA
-- $0330-035F [shadow] TIA
-- $0360-037F [shadow-partial] TIA
-- $0380-039F [shadow] 6532-PIA
-- $03A0-03BF [shadow] 6532-PIA
-- $03C0-03DF [shadow] 6532-PIA
-- $03E0-03FF [shadow] 6532-PIA
-- $0400-07FF [shadow] Repeat the pattern from $0000-03FF
-- $0800-0BFF [shadow] Repeat the pattern from $0000-03FF
-- $0C00-0FFF [shadow] Repeat the pattern from $0000-03FF
--
-- $1000-17FF Lower 2K Cartridge ROM (4K carts start here)
-- $1800-1FFF Upper 2K Cartridge ROM (2K carts go here) 
pureReadMemory :: MemoryType -> Word16 -> MonadAtari Word8
pureReadMemory ROM  addr = pureReadRom addr
pureReadMemory TIA  addr = readStella (addr `mod` 0x30) -- surprising!
pureReadMemory RIOT addr = readStella (0x280+(addr .&. 0x1f))
pureReadMemory RAM  addr = do
    atari <- ask
    let m = atari ^. ram
    liftIO $ readArray m (iz addr .&. 0x7f)

-- {-# INLINE pureWriteMemory #-}
pureWriteMemory :: MemoryType -> Word16 -> Word8 -> MonadAtari ()
pureWriteMemory TIA  addr v = writeStella (addr .&. 0x3f) v
pureWriteMemory RIOT addr v = writeStella (0x280+(addr .&. 0x1f)) v
pureWriteMemory ROM  addr v = pureWriteRom addr v
pureWriteMemory RAM  addr v = do
    atari <- ask
    let m = atari ^. ram
#if TRACE
    let r = atari ^. record
    i <- liftIO $ readIORef (atari ^. recordPtr)
#endif
    let realAddress = iz addr .&. 0x7f
    liftIO $ writeArray m realAddress v
#if TRACE
    liftIO $ writeArray r i (i8 realAddress)
    liftIO $ writeArray r (i+1) v
    liftIO $ writeIORef (atari ^. recordPtr) (i+2)
#endif


-- {-# INLINABLE dumpMemory #-}
dumpMemory :: MonadAtari ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    liftIO $ putStr $ "(PC) = "
    liftIO $ putStr $ showHex b0 "" ++ " "
    liftIO $ putStr $ showHex b1 "" ++ " "
    liftIO $ putStrLn $ showHex b2 ""
    let (_, mne, _) = disasm regPC [b0, b1, b2]
    liftIO $ putStrLn $ mne

-- {-# INLINABLE dumpRegisters #-}
dumpRegisters :: MonadAtari ()
dumpRegisters = do
    regPC <- getPC
    liftIO $ putStr $ " pc = " ++ showHex regPC ""
    regP <- getP
    liftIO $ do
        putStr $ " flags = " ++ showHex regP ""
        putStr $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
        putStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
        putStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
        putStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
        putStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
        putStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
        putStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    liftIO $ putStr $ ") A = " ++ showHex regA ""
    regX <- getX
    liftIO $ putStr $ " X = " ++ showHex regX ""
    regY <- getY
    liftIO $ putStrLn $ " Y = " ++ showHex regY ""
    regS <- getS
    liftIO $ putStrLn $ " N = " ++ showHex regS ""

-- {-# INLINABLE dumpState #-}
dumpState :: MonadAtari ()
dumpState = do
    dumpMemory
    dumpRegisters

graphicsDelay :: Int -> MonadAtari ()
graphicsDelay d = do
    n <- load ahead
    when (d > n) $ do
            stellaTickFor' (d-n)
            ahead @= d

{-
 - TIA Summary

6-bit Address
      Address Name
                76543210    Function
---+---------+-----------+------------------------------------------
00 |  VSYNC  |  ......1. |  vertical sync set-clear
01 |  VBLANK |  11....1. |  vertical blank set-clear
02 |  WSYNC  |  strobe   |  wait for leading edge of horizontal blank
03 |  RSYNC  |  strobe   |  reset horizontal sync counter
04 |  NUSIZ0 |  ..111111 |  number-size player-missile 0
05 |  NUSIZ1 |  ..111111 |  number-size player-missile 1
06 |  COLUP0 |  1111111. |  color-lum player 0
07 |  COLUP1 |  1111111. |  color-lum player 1
08 |  COLUPF |  1111111. |  color-lum playfield
09 |  COLUBK |  1111111. |  color-lum background
0A |  CTRLPF |  ..11.111 |  control playfield ball size & collisions
0B |  REFP0  |  ....1... |  reflect player 0
0C |  REFP1  |  ....1... |  reflect player 1
0D |  PF0    |  1111.... |  playfield register byte 0
0E |  PF1    |  11111111 |  playfield register byte 1
0F |  PF2    |  11111111 |  playfield register byte 2
10 |  RESP0  |  strobe   |  reset player 0
11 |  RESP1  |  strobe   |  reset player 1
12 |  RESM0  |  strobe   |  reset missile 0
13 |  RESM1  |  strobe   |  reset missile 1
14 |  RESBL  |  strobe   |  reset ball
15 |  AUDC0  |  ....1111 |  audio control 0
16 |  AUDC1  |  ...11111 |  audio control 1
17 |  AUDF0  |  ...11111 |  audio frequency 0
18 |  AUDF1  |  ....1111 |  audio frequency 1
19 |  AUDV0  |  ....1111 |  audio volume 0
1A |  AUDV1  |  ....1111 |  audio volume 1
1B |  GRP0   |  11111111 |  graphics player 0
1C |  GRP1   |  11111111 |  graphics player 1
1D |  ENAM0  |  ......1. |  graphics (enable) missile 0
1E |  ENAM1  |  ......1. |  graphics (enable) missile 1
1F |  ENABL  |  ......1. |  graphics (enable) ball
20 |  HMP0   |  1111.... |  horizontal motion player 0
21 |  HMP1   |  1111.... |  horizontal motion player 1
22 |  HMM0   |  1111.... |  horizontal motion missile 0
23 |  HMM1   |  1111.... |  horizontal motion missile 1
24 |  HMBL   |  1111.... |  horizontal motion ball
25 |  VDELP0 |  .......1 |  vertical delay player 0
26 |  VDELP1 |  .......1 |  vertical delay player 1
27 |  VDELBL |  .......1 |  vertical delay ball
28 |  RESMP0 |  ......1. |  reset missile 0 to player 0
29 |  RESMP1 |  ......1. |  reset missile 1 to player 1
2A |  HMOVE  |  strobe   |  apply horizontal motion
2B |  HMCLR  |  strobe   |  clear horizontal motion registers
2C |  CXCLR  |  strobe   |  clear collision latches
---+---------+-----------+------------------------------------------

-}

-- {- INLINABLE writeStella -}
writeStella :: Word16 -> Word8 -> MonadAtari ()
writeStella addr v = do
    when (addr <= 0x2c) $ do
        delays' <- view delays
        d <- liftIO $ readArray delays' addr
        graphicsDelay d

    case addr of
       0x00 -> stellaVsync v             -- VSYNC
       0x01 -> stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> nusiz0 @= v        -- NUSIZ0
       0x05 -> nusiz1 @= v        -- NUSIZ1
       0x06 -> (pcStep @-> pcColup0) >> colup0 @= v               -- COLUP0
       0x07 -> (pcStep @-> pcColup1) >> colup1 @= v               -- COLUP1
       0x08 -> (pcStep @-> pcColupf) >> colupf @= v               -- COLUPF
       0x09 -> (pcStep @-> pcColubk) >> colubk @= v               -- COLUBK
       0x0a -> ctrlpf @= v >> makePlayfield               -- CTRLPF
       0x0b -> refp0 @= v               -- REFP0
       0x0c -> refp1 @= v               -- REFP1
       -- I'm sure I read delay should be 3 for PF registers
       -- but that doesn't make sense to me.
       -- See docs/adventure_pf_timing.txt
       0x0d -> (pcStep @-> pcPf0) >> pf0 @= v >> makePlayfield    -- PF0
       0x0e -> (pcStep @-> pcPf1) >> pf1 @= v >> makePlayfield    -- PF1
       0x0f -> (pcStep @-> pcPf2) >> pf2 @= v >> makePlayfield    -- PF2
       0x10 -> (pcStep @-> pcResp0) >> hpos @-> ppos0 -- RESP0
       0x11 -> (pcStep @-> pcResp1) >> hpos @-> ppos1 -- RESP1
       0x12 -> (pcStep @-> pcResm0) >> hpos @-> mpos0 -- RESM0
       0x13 -> (pcStep @-> pcResm1) >> hpos @-> mpos1 -- RESM1
       0x14 -> (pcStep @-> pcResbl) >> load hpos >>= (return . max (picx+2)) >>= (bpos @=)  -- RESBL
       -- graphicsDelay of 1 chosen to stop spurious pixel in
       -- "CCE" in Freeway.
       0x1b -> do -- GRP0
                newGrp0 @= v
                newGrp1 @-> oldGrp1
       0x1c -> do -- GRP1
                newGrp1 @= v
                newGrp0 @-> oldGrp0
                newBall @-> oldBall
       0x1d -> enam0 @= v                -- ENAM0
       0x1e -> enam1 @= v                -- ENAM1
       0x1f -> newBall @= testBit v 1   -- ENABL
       0x20 -> hmp0 @= v                 -- HMP0
       0x21 -> hmp1 @= v                 -- HMP1
       0x22 -> hmm0 @= v                 -- HMM0
       0x23 -> hmm1 @= v                 -- HMM1
       0x24 -> hmbl @= v                 -- HMBL
       0x25 -> delayP0 @= testBit v 0   -- VDELP0
       0x26 -> delayP1 @= testBit v 0   -- VDELP1
       0x27 -> delayBall @= testBit v 0   -- VDELBL
       0x28 -> resmp0 @= v
       0x29 -> resmp1 @= v
       0x2a -> stellaHmove               -- HMOVE
       0x2b -> stellaHmclr               -- HMCLR
       0x2c -> stellaCxclr               -- CXCLR
       0x280 -> do
                 swcha @= v               -- XXX just added
       0x281 -> swacnt @= v
       0x294 -> startIntervalTimerN 1 v
       0x295 -> startIntervalTimerN 8 v
       0x296 -> startIntervalTimerN 64 v
       0x297 -> startIntervalTimerN 1024 v
       _ -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

-- add nanoseconds
addTime :: TimeSpec -> Int64 -> TimeSpec
addTime (TimeSpec a b) c =
    let d = b + c
    in if d >= 1000000000 then TimeSpec (a+1) (d-1000000000) else TimeSpec a d

renderDisplay :: MonadAtari ()
renderDisplay = do
    window <- view sdlWindow
    prog <- view glProg
    attrib <- view glAttrib
    parityRef <- view frameParity
    parity <- liftIO $ readIORef parityRef
    liftIO $ modifyIORef parityRef not
    tex' <- view tex
    lastTex' <- view lastTex
    ptr <- view textureData
    lastPtr <- view lastTextureData
    windowWidth' <- view windowWidth
    windowHeight' <- view windowHeight
    if parity
      then do
        liftIO $ updateTexture tex' ptr
        liftIO $ updateTexture lastTex' lastPtr
      else do
        liftIO $ updateTexture lastTex' ptr
        liftIO $ updateTexture tex' lastPtr
    liftIO $ draw window windowWidth' windowHeight' prog attrib

    nextFrameTimeRef <- view nextFrameTime
    nextFrameTime' <- liftIO $ readIORef nextFrameTimeRef
    t <- liftIO $ getTime Realtime
    let nt = addTime nextFrameTime' (1000000000 `div` 60)
    liftIO $ writeIORef nextFrameTimeRef nt
    let TimeSpec {sec=elapsedSec, nsec=elapsedNSec} = diffTimeSpec nextFrameTime' t
    let tt = fromIntegral elapsedSec+fromIntegral elapsedNSec/1000000000.0 :: Double
    when (tt > 0) $
        liftIO $ threadDelay $ floor $ 1000.0 * tt
    liftIO $ SDL.glSwapWindow window
    return ()

initHardware :: MonadAtari ()
initHardware = do
    store inpt0 0x80
    store inpt1 0x80
    store inpt2 0x80
    store inpt3 0x80
    store inpt4 0x80
    store inpt5 0x80
    store swcha 0b11111111
    store swacnt 0b00000000
    store swchb 0b00001011
    store xbreak (-1)
    store ybreak (-1)
    forM_ [0..3] $ \i-> forM_ [0..5] $ \j -> store (kbd i j) False
    pclo <- readMemory 0x1ffc
    pchi <- readMemory 0x1ffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)
    liftIO $ putStrLn $ "Starting at address: 0x" ++ showHex initialPC ""
    store pc initialPC
