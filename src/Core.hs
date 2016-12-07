{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- http://nesdev.com/6502_cpu.txt
-- http://www.zimmers.net/anonftp/pub/cbm/documents/chipdata/64doc

module Core(Emu6502(..), step, i8, i16, make16) where
--module Core where

import Data.Array.IO
import Data.Word
import Control.Monad.State
import Control.Lens
import Data.Bits
import Data.Bits.Lens
import Data.ByteString as B hiding (putStr, putStrLn, getLine, length)
import System.IO
import Data.Binary.Get
import Data.Binary
import Data.Int
import Numeric
import qualified Data.ByteString.Internal as BS (c2w, w2c)

class (Monad m, MonadIO m) => Emu6502 m where
    readMemory :: Word16 -> m Word8
    writeMemory :: Word16 -> Word8 -> m ()
    getPC :: m Word16
    tick :: Int -> m ()
    putC :: Bool -> m ()
    getC :: m Bool
    putZ :: Bool -> m ()
    getZ :: m Bool
    putI :: Bool -> m ()
    getI :: m Bool
    putD :: Bool -> m ()
    getD :: m Bool
    putB :: Bool -> m ()
    getB :: m Bool
    putV :: Bool -> m ()
    getV :: m Bool
    putN :: Bool -> m ()
    getN :: m Bool
    getA :: m Word8
    putA :: Word8 -> m ()
    getS :: m Word8
    putS :: Word8 -> m ()
    getX :: m Word8
    putX :: Word8 -> m ()
    getP :: m Word8
    putP :: Word8 -> m ()
    getY :: m Word8
    putY :: Word8 -> m ()
    putPC :: Word16 -> m ()
    addPC :: Int -> m ()
    illegal :: Word8 -> m ()

    debugStr :: Int -> String -> m ()
    debugStrLn :: Int -> String -> m ()

{-# INLINABLE dumpRegisters #-}
dumpRegisters :: Emu6502 m => m ()
dumpRegisters = do
    -- XXX bring clock back
    --tClock <- use clock
    --debugStr 9 $ "clock = " ++ show tClock
    regPC <- getPC
    debugStr 9 $ " pc = " ++ showHex regPC ""
    regP <- getP
    debugStr 9 $ " flags = " ++ showHex regP ""
    debugStr 9 $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
    debugStr 9 $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
    debugStr 9 $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
    debugStr 9 $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
    debugStr 9 $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
    debugStr 9 $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
    debugStr 9 $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    debugStr 9 $ ") A = " ++ showHex regA ""
    regX <- getX
    debugStr 9 $ " X = " ++ showHex regX ""
    regY <- getY
    debugStrLn 9 $ " Y = " ++ showHex regY ""
    regS <- getS
    debugStrLn 9 $ " N = " ++ showHex regS ""

{-# INLINABLE dumpMemory #-}
dumpMemory :: Emu6502 m => m ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    debugStr 9 $ "(PC) = "
    debugStr 9 $ showHex b0 "" ++ " "
    debugStr 9 $ showHex b1 "" ++ " "
    debugStrLn 9 $ showHex b2 ""

{-# INLINABLE dumpState #-}
dumpState :: Emu6502 m => m ()
dumpState = do
    dumpMemory
    dumpRegisters

{-# INLINE make16 #-}
make16 :: Word8 -> Word8 -> Word16
make16 lo hi = (i16 hi `shift` 8)+i16 lo

{-# INLINE incPC #-}
incPC :: Emu6502 m => m ()
incPC = addPC 1

{-# INLINABLE read16 #-}
read16 :: Emu6502 m => Word16 -> m Word16
read16 addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{-# INLINABLE read16tick #-}
read16tick :: Emu6502 m => Word16 -> m Word16
read16tick addr = do
    tick 1
    lo <- readMemory addr
    tick 1
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{-# INLINABLE read16zp #-}
read16zp :: Emu6502 m => Word8 -> m Word16
read16zp addr = do
    lo <- readMemory (i16 addr)
    hi <- readMemory (i16 addr+1)
    return $ make16 lo hi

{-# INLINABLE read16zpTick #-}
read16zpTick :: Emu6502 m => Word8 -> m Word16
read16zpTick addr = do
    tick 1
    lo <- readMemory (i16 addr)
    tick 1
    hi <- readMemory (i16 addr+1)
    return $ make16 lo hi

-- http://www.emulator101.com/6502-addressing-modes.html

{-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

{-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

{-# INLINE iz #-}
iz :: Integral a => a -> Int
iz = fromIntegral

-- Note, a 6502 performs a read or write *every* clock cycle
-- regardless of what instruction is being executed.

-- 6 clock cycles...
{-# INLINABLE writeIndirectX #-}
writeIndirectX :: Emu6502 m => Word8 -> m ()
writeIndirectX src = do
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
{-# INLINABLE writeZeroPage #-}
writeZeroPage :: Emu6502 m => Word8 -> m ()
writeZeroPage src = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    writeMemory (i16 addr) src
    incPC

-- 4 clock cycles
{-# INLINABLE writeAbsolute #-}
writeAbsolute :: Emu6502 m => Word8 -> m()
writeAbsolute src = do
    addr <- getPC >>= read16tick

    tick 1
    writeMemory addr src
    addPC 2

-- 6 clock cycles
{-# INLINABLE writeIndirectY #-}
writeIndirectY :: Emu6502 m => Word8 -> m ()
writeIndirectY src = do
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
{-# INLINABLE writeZeroPageX #-}
writeZeroPageX :: Emu6502 m => Word8 -> m ()
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
{-# INLINABLE writeZeroPageY #-}
writeZeroPageY :: Emu6502 m => Word8 -> m ()
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
{-# INLINABLE writeAbsoluteY #-}
writeAbsoluteY :: Emu6502 m => Word8 -> m ()
writeAbsoluteY src = do
    index <- getY
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrY, addrY) = halfSum addr index
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    addPC 2

-- 5 clock cycles
{-# INLINABLE writeAbsoluteX #-}
writeAbsoluteX :: Emu6502 m => Word8 -> m ()
writeAbsoluteX src = do
    index <- getX
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrX, addrX) = halfSum addr index
    discard $ readMemory halfAddrX

    tick 1
    writeMemory addrX src
    addPC 2

-- 6 clock cycles
{-# INLINABLE readIndirectX #-}
readIndirectX :: Emu6502 m => m Word8
readIndirectX = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    addr <- read16zpTick (addr+index)

    tick 1
    incPC
    readMemory addr

-- 3 clock cycles
{-# INLINABLE readZeroPage #-}
readZeroPage :: Emu6502 m => m Word8
readZeroPage = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)
    incPC
    return src

-- 2 clock cycles
{-# INLINABLE readImmediate #-}
readImmediate :: Emu6502 m => m Word8
readImmediate = do
    tick 1
    src <- getPC >>= readMemory
    incPC
    return src

-- XXX consider applicable ops like *>
-- 4 clock cycles
{-# INLINABLE readAbsolute #-}
readAbsolute :: Emu6502 m => m Word8
readAbsolute = do
    p0 <- getPC
    src <- (read16tick p0 <* tick 1) >>= readMemory
    addPC 2
    return src

-- 5-6 clock cycles
{-# INLINABLE readIndirectY #-}
readIndirectY :: Emu6502 m => m Word8
readIndirectY = do
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
{-# INLINABLE readZeroPageX #-}
readZeroPageX :: Emu6502 m => m Word8
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
{-# INLINABLE readZeroPageY #-}
readZeroPageY :: Emu6502 m => m Word8
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

{-# INLINABLE halfSignedSum #-}
halfSignedSum :: Word16 -> Word8 -> (Word16, Word16)
halfSignedSum addr index = 
    let fullSum = if index < 0x80 then addr+i16 index else addr+i16 index-0x100
    in (make16 (lo addr+index) (hi addr), fullSum)

-- 4-5 clock cycles
{-# INLINABLE readAbsoluteX #-}
readAbsoluteX :: Emu6502 m => m Word8
readAbsoluteX = do
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
{-# INLINABLE readAbsoluteY #-}
readAbsoluteY :: Emu6502 m => m Word8
readAbsoluteY = do
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
{-# INLINABLE ins_bra #-}
ins_bra :: Emu6502 m => m Bool -> Bool -> m ()
ins_bra getFlag value = do
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
{-# INLINABLE ins_set #-}
ins_set :: Emu6502 m => (Bool -> m ()) -> Bool -> m ()
ins_set putFlag value = do
    tick 1
    discard $ getPC >>= readMemory
    putFlag value

-- 2 clock cycles
{-# INLINABLE ins_nop #-}
ins_nop :: Emu6502 m => m ()
ins_nop = do
    tick 1
    discard $ getPC >>= readMemory

-- 3 clock cycles
{-# INLINABLE ins_jmp #-}
ins_jmp :: Emu6502 m => m ()
ins_jmp = getPC >>= read16tick >>= putPC

{-# INLINE nonwhite #-}
nonwhite :: Word8 -> String
nonwhite ra | ra < 32 = "()"
nonwhite ra = "'" ++ [BS.w2c ra] ++ "'"

-- 5 clock cycles
-- NB address wraps around in page XXX
-- Not correct here.
-- Looks like the torture test might not catch this.
-- Aha! That's why ALIGN is used before addresses!
{-# INLINABLE ins_jmp_indirect #-}
ins_jmp_indirect :: Emu6502 m => m ()
ins_jmp_indirect = do
    getPC >>= read16tick >>= read16tick >>= putPC

{-# INLINABLE uselessly #-}
uselessly :: Emu6502 m => m () -> m ()
uselessly = id

-- 5 clock cycles
{-# INLINABLE withZeroPage #-}
withZeroPage :: Emu6502 m => (Word8 -> m Word8) -> m ()
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
{-# INLINABLE withAccumulator #-}
withAccumulator :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAccumulator op = do
    tick 1
    discard $ getPC >>= readMemory
    getA >>= op >>= putA

-- 6 clock cycles
{-# INLINE withAbsolute #-}
withAbsolute :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsolute op = do
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
withZeroPageX :: Emu6502 m => (Word8 -> m Word8) -> m ()
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

-- 6 clock cycles
withZeroPageY :: Emu6502 m => (Word8 -> m Word8) -> m ()
withZeroPageY op = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory
    let addrY = addr+index

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    src <- readMemory (i16 addrY)

    tick 1
    writeMemory (i16 addrY) src

    tick 1
    dst <- op src
    writeMemory (i16 addrY) dst
    incPC
 
-- 7 clock cycles
{-# INLINE withAbsoluteX #-}
withAbsoluteX :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsoluteX op = do
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

-- 7 clock cycles
{-# INLINE withAbsoluteY #-}
withAbsoluteY :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsoluteY op = do
    p0 <- getPC
    index <- getY
    addr <- read16tick p0

    let (halfAddrY, addrY) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrY

    tick 1
    src <- readMemory addrY

    tick 1
    uselessly $ writeMemory addrY src

    tick 1
    addPC 2
    dst <- op src
    writeMemory addrY dst

{-# INLINABLE getData01 #-}
getData01 :: Emu6502 m => Word8 -> m Word8
getData01 bbb = do
    case bbb of
        0b000 -> readIndirectX
        0b001 -> readZeroPage
        0b010 -> readImmediate
        0b011 -> readAbsolute
        0b100 -> readIndirectY
        0b101 -> readZeroPageX
        0b110 -> readAbsoluteY
        0b111 -> readAbsoluteX

{-# INLINABLE getData02 #-}
getData02 :: Emu6502 m =>
              Word8 -> Bool ->
              (Word8 -> m ()) ->
              m ()
getData02 bbb useY op = case bbb of
    0b000 -> readImmediate >>= op
    0b001 -> readZeroPage >>= op
    0b010 -> error "Must write back to A"
    0b011 -> readAbsolute >>= op
    0b101 -> if useY
                then readZeroPageY >>= op
                else readZeroPageX >>= op
    0b111 -> if useY
            then readAbsoluteY >>= op
            else readAbsoluteX >>= op

    otherwise -> error "Unknown addressing mode"

-- Need to separate W and (RW/R) XXX XXX XXX
{-# INLINABLE withData02 #-}
withData02 :: Emu6502 m =>
              Word8 -> Bool ->
              (Word8 -> m Word8) ->
              m ()
withData02 bbb useY op = case bbb of
    0b000 -> getPC >>= readMemory . ((-) 1) >>= illegal -- XXX reread mem. Should check in caller.
    0b001 -> withZeroPage op
    0b010 -> withAccumulator op
    0b011 -> withAbsolute op
    0b101 -> if useY then withZeroPageY op else withZeroPageX op
    0b111 -> if useY then withAbsoluteY op else withAbsoluteX op

    otherwise -> error "Unknown addressing mode"

{-# INLINABLE putData02 #-}
putData02 :: Emu6502 m => Word8 -> Bool -> Word8 -> m ()
putData02 bbb useY src = case bbb of
    0b000 -> error "No write immediate"
    0b001 -> writeZeroPage src
    0b010 -> error "No write accumulator"
    0b011 -> writeAbsolute src
    0b101 -> if useY then writeZeroPageY src else writeZeroPageX src
    0b111 -> if useY then writeAbsoluteY src else writeAbsoluteX src

    otherwise -> error "Unknown addressing mode"

{-# INLINABLE putData01 #-}
putData01 :: Emu6502 m => Word8 -> Word8 -> m ()
putData01 bbb src = do
    p0 <- getPC
    case bbb of
        0b000 -> writeIndirectX src -- (zero page, X)
        0b001 -> writeZeroPage src
        0b010 -> readMemory (p0-1) >>= illegal -- XXX imm. check in caller
        0b011 -> writeAbsolute src
        0b100 -> writeIndirectY src -- (zero page), Y
        0b101 -> writeZeroPageX src
        0b110 -> writeAbsoluteY src
        0b111 -> writeAbsoluteX src

{-# INLINABLE setN #-}
setN :: Emu6502 m => Word8 -> m ()
setN r = putN $ r >= 0x80

{-# INLINABLE setZ #-}
setZ :: Emu6502 m => Word8 -> m ()
setZ r = putZ $ r == 0

{-# INLINABLE setNZ #-}
setNZ :: Emu6502 m => Word8 -> m Word8
setNZ r = setN r >> setZ r >> return r

{-# INLINABLE setNZ_ #-}
setNZ_ :: Emu6502 m => Word8 -> m ()
setNZ_ r = setN r >> setZ r

{-# INLINABLE op_ora #-}
op_ora :: Emu6502 m => Word8 -> m ()
op_ora bbb = do
    src <- getData01 bbb
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setNZ_ newA

{-# INLINABLE op_and #-}
op_and :: Emu6502 m => Word8 -> m ()
op_and bbb = do
    src <- getData01 bbb
    getA >>= setNZ . (src .&.) >>= putA

{-# INLINABLE op_xor #-}
op_xor :: Emu6502 m => Word8 -> m ()
op_xor bbb = do
    src <- getData01 bbb
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    void $ setNZ newA

{-# INLINABLE op_lda #-}
op_lda :: Emu6502 m => Word8 -> m ()
op_lda bbb = do
    getData01 bbb >>= setNZ >>= putA

{-# INLINABLE op_sta #-}
op_sta :: Emu6502 m => Word8 -> m ()
op_sta bbb = getA >>= putData01 bbb

{-# INLINABLE op_adc #-}
op_adc :: Emu6502 m => Word8 -> m ()
op_adc bbb = do
    src <- getData01 bbb
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

{-# INLINABLE op_sbc #-}
op_sbc :: Emu6502 m => Word8 -> m ()
op_sbc bbb = do
    src <- getData01 bbb
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    setNZ $ i8 newA
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

{-# INLINABLE op_cmp #-}
op_cmp :: Emu6502 m => Word8 -> m ()
op_cmp bbb = do
    src <- getData01 bbb
    oldA <- getA
    let new = i16 oldA-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

{-# INLINABLE op_asl #-}
op_asl :: Emu6502 m => Word8 -> m ()
op_asl bbb = withData02 bbb False $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setNZ new

{-# INLINABLE op_rol #-}
op_rol :: Emu6502 m => Word8 -> m ()
op_rol bbb = withData02 bbb False $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setNZ new
    return new

{-# INLINABLE op_lsr #-}
op_lsr :: Emu6502 m => Word8 -> m ()
op_lsr bbb = withData02 bbb False $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

{-# INLINABLE op_ror #-}
op_ror :: Emu6502 m => Word8 -> m ()
op_ror bbb = withData02 bbb False $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setNZ new

{-# INLINABLE ins_stx #-}
ins_stx :: Emu6502 m => Word8 -> m ()
ins_stx bbb = getX >>= putData02 bbb True

{-# INLINABLE op_ldx #-}
op_ldx :: Emu6502 m => Word8 -> m ()
op_ldx bbb = getData02 bbb True $ \src -> do
    putX src
    setNZ_ src

{-# INLINABLE op_dec #-}
op_dec :: Emu6502 m => Word8 -> m ()
op_dec bbb = withData02 bbb False $ \src -> setNZ (src-1)

{-# INLINABLE op_inc #-}
op_inc :: Emu6502 m => Word8 -> m ()
op_inc bbb = withData02 bbb False $ \src -> setNZ (src+1)

{-# INLINABLE op_bit #-}
op_bit :: Emu6502 m => Word8 -> m ()
op_bit bbb = getData02 bbb False $ \src -> do
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src

{-# INLINABLE ins_sty #-}
ins_sty :: Emu6502 m => Word8 -> m ()
ins_sty bbb = getY >>= putData02 bbb False

{-# INLINABLE op_ldy #-}
op_ldy :: Emu6502 m => Word8 -> m ()
op_ldy bbb = getData02 bbb False $ \src -> do
    putY src
    setNZ_ src

{-# INLINABLE op_cpx #-}
op_cpx :: Emu6502 m => Word8 -> m ()
op_cpx bbb = getData02 bbb False $ \src -> do
    rx <- getX
    let new = i16 rx-i16 src
    setNZ $ i8 new
    putC $ new < 0x100

{-# INLINABLE op_cpy #-}
op_cpy :: Emu6502 m => Word8 -> m ()
op_cpy bbb = getData02 bbb False $ \src -> do
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

-- 2 clock cycles
{-# INLINABLE ins_txs #-}
ins_txs :: Emu6502 m => m ()
ins_txs = do
    tick 1
    discard $ getPC >>= readMemory
    getX >>= putS

-- 2 clock cycles
{-# INLINABLE ins_transfer #-}
ins_transfer :: Emu6502 m =>
                     m Word8 -> (Word8 -> m ()) ->
                     m ()
ins_transfer getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    getReg >>= setNZ >>= putReg

-- 2 clock cycles
{-# INLINABLE ins_incr #-}
ins_incr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_incr getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    v0 <- getReg
    let v1 = v0+1
    setNZ v1
    putReg v1

-- 2 clock cycles
{-# INLINABLE ins_decr #-}
ins_decr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_decr getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    v0 <- getReg
    let v1 = v0-1
    setNZ v1
    putReg v1

discard :: Emu6502 m => m Word8 -> m ()
discard = void

-- 7 clock cycles
{-# INLINABLE ins_brk #-}
ins_brk :: Emu6502 m => m ()
ins_brk = do
    p0 <- getPC
    incPC
    discard $ readMemory p0

    p0 <- getPC
    incPC
    push $ hi p0

    incPC
    push $ lo p0

    putB True
    incPC
    getP >>= push . (.|. 0x20) -- always on bit
    putI True

    read16tick 0xfffe >>= putPC -- irq/brk XXX

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
-- XXX not supported correctly for now
{-# INLINABLE irq #-}
irq :: Emu6502 m => m ()
irq = do
    fi <- getI
    if not fi
        then nmi False
        else return ()

{-# INLINABLE push #-}
push :: Emu6502 m => Word8 -> m ()
push v = do
    sp <- getS
    writeMemory (0x100+i16 sp) v
    putS (sp-1)

{-# INLINABLE pull #-}
pull :: Emu6502 m => m Word8
pull = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemory (0x100+i16 sp')

-- 3 clock cycles
{-# INLINABLE ins_pha #-}
ins_pha ::Emu6502 m => m ()
ins_pha = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getA >>= push

-- 3 clock cycles
{-# INLINABLE ins_php #-}
ins_php :: Emu6502 m => m ()
ins_php = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getP >>= push . (.|. 0x30)

-- 4 clock cycles
{-# INLINABLE ins_plp #-}
ins_plp :: Emu6502 m => m ()
ins_plp = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= putP

-- 4 clock cycles
{-# INLINABLE ins_pla #-}
ins_pla :: Emu6502 m => m ()
ins_pla = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= setNZ >>= putA

{-# INLINABLE lo #-}
lo :: Word16 -> Word8
lo = i8

{-# INLINABLE hi #-}
hi :: Word16 -> Word8
hi a = i8 (a `shift` (-8))

{-# INLINABLE nmi #-}
nmi :: Emu6502 m => Bool -> m ()
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
{-# INLINABLE ins_rti #-}
ins_rti :: Emu6502 m => m ()
ins_rti = do
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
{-# INLINABLE ins_jsr #-}
ins_jsr :: Emu6502 m => m ()
ins_jsr = do
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
{-# INLINABLE ins_rts #-}
ins_rts :: Emu6502 m => m ()
ins_rts = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    p0 <- make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull)
    
    tick 1
    discard $ readMemory p0
    putPC (p0+1)

{-# INLINABLE step #-}
step :: Emu6502 m => m ()
step = do
    --dumpState
    p0 <- getPC
    --if p0 == 0x3781 then debug .= True else return () -- XXX
    if p0 == 0x400 then liftIO $ putStrLn "Started!!!" else return ()
    if p0 == 0x3770 then liftIO $ putStrLn "Passed!!!" else return ()
    tick 1
    i <- readMemory p0
    incPC
    case i of
        0x00 -> ins_brk
        0x08 -> ins_php
        0x10 -> ins_bra getN False
        0x18 -> ins_set putC False
        0x20 -> ins_jsr
        0x28 -> ins_plp
        0x30 -> ins_bra getN True
        0x38 -> ins_set putC True
        0x40 -> ins_rti
        0x48 -> ins_pha
        0x50 -> ins_bra getV False
        0x58 -> ins_set putI False
        0x60 -> ins_rts
        0x68 -> ins_pla
        0x70 -> ins_bra getV True
        0x78 -> ins_set putI True
        0x88 -> ins_decr getY putY
        0x8a -> ins_transfer getX putA
        0x90 -> ins_bra getC False
        0x98 -> ins_transfer getY putA
        0x9a -> ins_txs
        0xa8 -> ins_transfer getA putY
        0xaa -> ins_transfer getA putX
        0xb0 -> ins_bra getC True
        0xb8 -> ins_set putV False
        0xba -> ins_transfer getS putX
        0xc8 -> ins_incr getY putY
        0xca -> ins_decr getX putX
        0xd0 -> ins_bra getZ False
        0xd8 -> ins_set putD False
        0xe8 -> ins_incr getX putX
        0xea -> ins_nop
        0xf0 -> ins_bra getZ True
        0xf8 -> ins_set putD True

        otherwise -> do
            let cc = i .&. 0b11
            case cc of
                0b00 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of
                        0b001 -> op_bit bbb
                        0b010 -> ins_jmp
                        0b011 -> ins_jmp_indirect
                        0b100 -> ins_sty bbb
                        0b101 -> op_ldy bbb
                        0b110 -> op_cpy bbb
                        0b111 -> op_cpx bbb

                        otherwise -> illegal i

                0b01 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> op_ora bbb
                        0b001 -> op_and bbb
                        0b010 -> op_xor bbb
                        0b011 -> op_adc bbb
                        0b100 -> op_sta bbb
                        0b101 -> op_lda bbb
                        0b110 -> op_cmp bbb
                        0b111 -> op_sbc bbb

                        otherwise -> illegal i
                0b10 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> op_asl bbb
                        0b001 -> op_rol bbb
                        0b010 -> op_lsr bbb
                        0b011 -> op_ror bbb
                        0b100 -> ins_stx bbb
                        0b101 -> op_ldx bbb
                        0b110 -> op_dec bbb
                        0b111 -> op_inc bbb

                otherwise -> illegal i
    dumpState
    return ()
