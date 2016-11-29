{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Binary
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Array.IO
import Data.Array.Unboxed
import Data.Binary
import Data.Binary.Get
import Data.Bits hiding (bit)
import Data.Bits.Lens
import Data.ByteString as B hiding (putStr, putStrLn, getLine, length, elem, map, reverse)
import Data.Char
import Data.Int
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Numeric
import SDL.Event
import SDL.Input.Keyboard
import SDL.Vect
import SDL.Video.Renderer
import System.Console.CmdArgs hiding ((+=))
import System.IO
--import System.Random
import TIAColors
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified SDL

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

-- Deliberately non-functioning INLINEs
{- INLINE dumpRegisters -}
dumpRegisters :: Emu6502 m => m ()
dumpRegisters = do
    -- XXX bring clock back
    --tClock <- use clock
    --debugStr $ "clock = " ++ show tClock
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

{- INLINE dumpMemory -}
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

{- INLINE dumpState -}
dumpState :: Emu6502 m => m ()
dumpState = do
    dumpMemory
    dumpRegisters

{- INLINE make16 -}
make16 :: Word8 -> Word8 -> Word16
make16 lo hi = (i16 hi `shift` 8)+i16 lo

{- INLINE read16 -}
read16 :: Emu6502 m => Word16 -> m Word16
read16 addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{- INLINE read16zp -}
read16zp :: Emu6502 m => Word8 -> m Word16
read16zp addr = do
    lo <- readMemory (i16 addr)
    hi <- readMemory (i16 addr+1)
    return $ make16 lo hi

-- http://www.emulator101.com/6502-addressing-modes.html

{- INLINE i8 -}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

{- INLINE i16 -}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

{- INLINE iz -}
iz :: Integral a => a -> Int
iz = fromIntegral

{-
aboutToBrk :: Monad6502 Bool
aboutToBrk = do
    p0 <- getPC
    ins <- readMemory (fromIntegral p0)
    return $ ins == 0x00
-}

{- INLINE writeIndirectX -}
writeIndirectX :: Emu6502 m => Word8 -> m ()
writeIndirectX src = do
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory (p0+1)
    addr <- read16zp (zpAddr+offsetX)
    writeMemory addr src
    putPC $ p0+2
    tick 6

{- INLINE writeZeroPage -}
writeZeroPage :: Emu6502 m => Word8 -> m ()
writeZeroPage src = do
    p0 <- getPC
    addr <- readMemory (p0+1)
    writeMemory (i16 addr) src
    putPC $ p0+2
    tick 3

{- INLINE writeAbsolute -}
writeAbsolute :: Emu6502 m => Word8 -> m()
writeAbsolute src = do
    p0 <- getPC
    addr <- read16 (p0+1)
    writeMemory addr src
    putPC $ p0+3
    tick 4

{- INLINE writeIndirectY -}
writeIndirectY :: Emu6502 m => Word8 -> m ()
writeIndirectY src = do
    p0 <- getPC
    offsetY <- getY
    addr <- readMemory (p0+1) >>= read16zp
    writeMemory (addr+i16 offsetY) src
    putPC $ p0+2
    tick 6

{- INLINE writeZeroPageX -}
writeZeroPageX :: Emu6502 m => Word8 -> m ()
writeZeroPageX src = do
    p0 <- getPC
    offsetX <- getX
    zpAddr <- readMemory (p0+1)
    writeMemory (i16 $ zpAddr+offsetX) src
    putPC $ p0+2
    tick 4

{- INLINE writeAbsoluteY -}
writeAbsoluteY :: Emu6502 m => Word8 -> m ()
writeAbsoluteY src = do
    p0 <- getPC
    offsetY <- getY
    baseAddr <- read16 (p0+1)
    writeMemory (baseAddr+i16 offsetY) src
    putPC $ p0+3
    tick 5

{- INLINE writeAbsoluteX -}
writeAbsoluteX :: Emu6502 m => Word8 -> m ()
writeAbsoluteX src = do
    p0 <- getPC
    offsetX <- getX
    baseAddr <- read16 (p0+1)
    writeMemory (baseAddr+i16 offsetX) src
    putPC $ p0+3
    tick 5

{- INLINE putData -}
putData :: Emu6502 m => Word8 -> Word8 -> m ()
putData bbb src = do
    p0 <- getPC
    case bbb of
        0b000 -> writeIndirectX src -- (zero page, X)
        0b001 -> writeZeroPage src
        0b010 -> readMemory p0 >>= illegal -- XXX imm. check in caller
        0b011 -> writeAbsolute src
        0b100 -> writeIndirectY src -- (zero page), Y
        0b101 -> writeZeroPageX src
        0b110 -> writeAbsoluteY src
        0b111 -> writeAbsoluteX src

{- INLINE getData -}
getData :: Emu6502 m => Word8 -> m Word8
getData bbb = do
    p0 <- getPC
    case bbb of
        -- (zero page, X)
        0b000 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            let addrAddr = zpAddr+offsetX
            addr <- read16zp addrAddr
            src <- readMemory addr
            putPC $ p0+2
            tick 6
            return src
        -- zero page
        0b001 -> do
            addr <- readMemory (p0+1)
            src <- readMemory (i16 addr)
            putPC $ p0+2
            tick 3
            return src
        -- immediate
        0b010 -> do
            src <- readMemory (p0+1)
            putPC $ p0+2
            tick 2
            return src
        -- absolute
        0b011 -> do
            src <- read16 (p0+1) >>= readMemory
            putPC $ p0+3
            tick 4
            return src
        -- (zero page), Y
        0b100 -> do
            offsetY <- getY
            addr <- readMemory (p0+1) >>= read16zp
            let newAddr = addr+i16 offsetY
            let carry = (newAddr .&. 0xff00) /= (addr .&. 0xff00)
            src <- readMemory (addr+i16 offsetY)
            putPC (p0+2)
            tick $ if carry then 6 else 5
            return src
        -- zero page, X
        0b101 -> do
            offsetX <- getX
            zpAddr <- readMemory (p0+1)
            src <- readMemory (i16 $ zpAddr+offsetX)
            putPC (p0+2)
            tick 4
            return src
        -- absolute, Y
        0b110 -> do
            offsetY <- getY
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetY
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            src <- readMemory addr
            putPC $ p0+3
            tick $ if carry then 5 else 4
            return src
        -- absolute, X
        0b111 -> do
            offsetX <- getX
            baseAddr <- read16 (p0+1)
            let addr = baseAddr+i16 offsetX
            let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
            src <- readMemory addr
            putPC $ p0+3
            tick $ if carry then 5 else 4
            return src

{- INLINE ins_bra -}
ins_bra :: Emu6502 m => m Bool -> Bool -> m ()
ins_bra getFlag value = do
    f <- getFlag
    p0 <- getPC
    let oldP = p0+2
    if value && f || not value && not f
        then do
            debugStrLn 9 "Taking branch"
            offset <- readMemory (p0+1) -- XXX or ^^^
            let newP = if offset < 0x80 then oldP+i16 offset else oldP+i16 offset-0x100
            tick $ if newP .&. 0xff00 == oldP .&. 0xff00 then 3 else 4
            putPC newP
        else do
            debugStrLn 9 "Not taking branch"
            tick 2
            putPC oldP

{- INLINE ins_set -}
ins_set :: Emu6502 m => (Bool -> m ()) -> Bool -> m ()
ins_set putFlag value = do
    putFlag value
    getPC >>= putPC . (+1)
    tick 2

{- INLINE ins_nop -}
ins_nop :: Emu6502 m => m ()
ins_nop = addPC 1 >> tick 2

{- INLINE ins_jmp -}
ins_jmp :: Emu6502 m => m ()
ins_jmp = do
    addr <- getPC >>= read16 . (+1)
    putPC addr
    tick 3

{- INLINE nonwhite -}
nonwhite :: Word8 -> String
nonwhite ra | ra < 32 = "()"
nonwhite ra = "'" ++ [BS.w2c ra] ++ "'"

{- INLINE ins_jmp_indirect -}
ins_jmp_indirect :: Emu6502 m => m ()
ins_jmp_indirect = do
    --p0 <- getPC
    --addr <- read16 (p0+1)
    --liftIO $ putStrLn $ "Indirect jmp 0x" ++ showHex addr ""
    getPC >>= read16 . (+1) >>= read16 >>= putPC
    tick 5

-- Need to separate R/W/RW XXX
{- INLINE withData02 -}
withData02 :: Emu6502 m =>
              Word8 -> Bool -> Bool ->
              (Word8 -> m Word8) ->
              m ()
withData02 bbb write useY op = case bbb of
    -- immediate
    0b000 -> if write
                then do
                    getPC >>= readMemory >>= illegal -- XXX reread mem. Should check in caller.
                else do
                    p0 <- getPC
                    src <- readMemory (p0+1)
                    putPC $ p0+2
                    op src
                    tick 2
    -- zero page
    0b001 -> do
        p0 <- getPC
        addr <- readMemory (p0+1)
        if write
            then do
                readMemory (i16 addr) >>= op >>= writeMemory (i16 addr)
                tick 5
            else do
                readMemory (i16 addr) >>= op
                tick 3
        putPC $ p0+2
    -- accumulator -- XXX
    0b010 -> do
        p0 <- getPC
        getA >>= op >>= putA
        putPC $ p0+1
        if write
            then tick 2
            else error "Must write back to A"
    -- absolute
    0b011 -> do
        p0 <- getPC
        addr <- read16 (p0+1)
        dst <- readMemory addr >>= op
        putPC $ p0+3
        if write
            then do
                writeMemory addr dst
                tick 6
            else tick 4
    -- zero page, X
    0b101 -> do
        p0 <- getPC
        offsetX <- if useY then getY else getX
        zpAddr <- readMemory (p0+1)
        let addr = zpAddr+offsetX
        src <- readMemory (i16 addr)
        putPC $ p0+2
        dst <- op src
        if write
            then do
                writeMemory (i16 addr) dst
                tick 6
            else tick 4
    -- absolute, X
    0b111 -> do
        p0 <- getPC
        offsetX <- if useY then getY else getX
        baseAddr <- read16 (p0+1)
        let addr = baseAddr+i16 offsetX
        let carry = (addr .&. 0xff00) /= (baseAddr .&. 0xff00)
        src <- readMemory addr
        putPC $ p0+3
        dst <- op src
        if write
            then do
                writeMemory addr dst
                tick 7
            else
                tick $ if carry then 5 else 4

    otherwise -> error "Unknown addressing mode"

{- INLINE setN -}
setN :: Emu6502 m => Word8 -> m ()
setN r = putN $ r >= 0x80

{- INLINE setZ -}
setZ :: Emu6502 m => Word8 -> m ()
setZ r = putZ $ r == 0

{- INLINE ins_ora -}
ins_ora :: Emu6502 m => Word8 -> m ()
ins_ora bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setN newA
    setZ newA
    debugStrLn 9 $ "A = " ++ show newA

{- INLINE ins_and -}
ins_and :: Emu6502 m => Word8 -> m ()
ins_and bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA .&. src
    putA newA
    setN newA
    setZ newA
    debugStrLn 9 $ "A = " ++ show newA

{- INLINE ins_xor -}
ins_xor :: Emu6502 m => Word8 -> m ()
ins_xor bbb = do
    src <- getData bbb
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    setN newA
    setZ newA
    debugStrLn 9 $ "A = " ++ show newA

{- INLINE ins_lda -}
ins_lda :: Emu6502 m => Word8 -> m ()
ins_lda bbb = do
    debugStrLn 9 $ "LDA instruction with address mode " ++ showHex bbb ""
    newA <- getData bbb
    putA newA
    setN newA
    setZ newA
    debugStrLn 9 $ "A = " ++ show newA

{- INLINE ins_sta -}
ins_sta :: Emu6502 m => Word8 -> m ()
ins_sta bbb = getA >>= putData bbb

{- INLINE ins_adc -}
ins_adc :: Emu6502 m => Word8 -> m ()
ins_adc bbb = do
    src <- getData bbb
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

{- INLINE ins_sbc -}
ins_sbc :: Emu6502 m => Word8 -> m ()
ins_sbc bbb = do
    src <- getData bbb
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    setN $ i8 newA
    setZ $ i8 newA
    putV $ (((i16 oldA `xor` i16 src) .&. 0x80) /= 0) && (((i16 oldA `xor` newA) .&. 0x80) /= 0)
    decimal <- getD
    if decimal
        then do
            debugStrLn 9 $ "Decimal subtract oldA ="++showHex oldA "" ++ " src=" ++ showHex src ""
            debugStrLn 9 $ "unadjusted = " ++ showHex newA ""
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
    debugStrLn 9 $ "A = " ++ show newA

{- INLINE ins_cmp -}
ins_cmp :: Emu6502 m => Word8 -> m ()
ins_cmp bbb = do
    src <- getData bbb
    oldA <- getA
    let new = i16 oldA-i16 src
    putC $ new < 0x100
    setN $ i8 new
    setZ $ i8 new

{- INLINE ins_asl -}
ins_asl :: Emu6502 m => Word8 -> m ()
ins_asl bbb = withData02 bbb True False $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setN new
    setZ new
    return new

{- INLINE ins_rol -}
ins_rol :: Emu6502 m => Word8 -> m ()
ins_rol bbb = withData02 bbb True False $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setN new
    setZ new
    return new

{- INLINE ins_lsr -}
ins_lsr :: Emu6502 m => Word8 -> m ()
ins_lsr bbb = withData02 bbb True False $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

{- INLINE ins_ror -}
ins_ror :: Emu6502 m => Word8 -> m ()
ins_ror bbb = withData02 bbb True False $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setN new
    setZ new
    return new

{- INLINE ins_stx -}
ins_stx :: Emu6502 m => Word8 -> m ()
ins_stx bbb = withData02 bbb True True $ \_ -> getX

{- INLINE ins_ldx -}
ins_ldx :: Emu6502 m => Word8 -> m ()
ins_ldx bbb = withData02 bbb False True $ \src -> do
    putX src
    setN src
    setZ src
    return 0 -- Unused, I hope

{- INLINE ins_dec -}
ins_dec :: Emu6502 m => Word8 -> m ()
ins_dec bbb = withData02 bbb True False $ \src -> do
    let new = src-1
    setN new
    setZ new
    return new

{- INLINE ins_inc -}
ins_inc :: Emu6502 m => Word8 -> m ()
ins_inc bbb = withData02 bbb True False $ \src -> do
    let new = src+1
    setN new
    setZ new
    return new

{- INLINE ins_bit -}
ins_bit :: Emu6502 m => Word8 -> m ()
ins_bit bbb = withData02 bbb False False $ \src -> do
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src
    return 0 -- unused

{- INLINE ins_sty -}
ins_sty :: Emu6502 m => Word8 -> m ()
ins_sty bbb = withData02 bbb True False $ \_ -> getY

{- INLINE ins_ldy -}
ins_ldy :: Emu6502 m => Word8 -> m ()
ins_ldy bbb = withData02 bbb False False $ \src -> do
    putY src
    setN src
    setZ src
    return 0 -- Unused, I hope

{- INLINE ins_cpx -}
ins_cpx :: Emu6502 m => Word8 -> m ()
ins_cpx bbb = withData02 bbb False False $ \src -> do
    rx <- getX
    let new = i16 rx-i16 src
    setN $ i8 new
    setZ $ i8 new
    putC $ new < 0x100
    return 0 -- unused

{- INLINE ins_cpy -}
ins_cpy :: Emu6502 m => Word8 -> m ()
ins_cpy bbb = withData02 bbb False False $ \src -> do
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setN $ i8 new
    setZ $ i8 new
    return 0 -- unused

{- INLINE ins_txs -}
ins_txs :: Emu6502 m => m ()
ins_txs = do
    getX >>= putS
    addPC 1
    tick 2

{- INLINE ins_transfer -}
ins_transfer :: Emu6502 m =>
                     m Word8 -> (Word8 -> m ()) ->
                     m ()
ins_transfer getReg putReg = do
    v0 <- getReg
    putReg v0
    setN v0
    setZ v0
    addPC 1
    tick 2

{- INLINE ins_incr -}
ins_incr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_incr getReg putReg = do
    v0 <- getReg
    let v1 = v0+1
    setN v1
    setZ v1
    putReg v1
    addPC 1
    tick 2

{- INLINE ins_decr -}
ins_decr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
ins_decr getReg putReg = do
    v0 <- getReg
    let v1 = v0-1
    setN v1
    setZ v1
    putReg v1
    addPC 1
    tick 2

{- INLINE ins_brk -}
ins_brk :: Emu6502 m => m ()
ins_brk = do
    addPC 2
    putB True
    nmi True

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
{- INLINE irq -}
irq :: Emu6502 m => m ()
irq = do
    fi <- getI
    if not fi
        then nmi False
        else return ()

{- INLINE push -}
push :: Emu6502 m => Word8 -> m ()
push v = do
    sp <- getS
    writeMemory (0x100+i16 sp) v
    putS (sp-1)

{- INLINE pull -}
pull :: Emu6502 m => m Word8
pull = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemory (0x100+i16 sp')

{- INLINE ins_pha -}
ins_pha ::Emu6502 m => m ()
ins_pha = do
    getA >>= push
    addPC 1
    tick 3

{- INLINE ins_php -}
ins_php :: Emu6502 m => m ()
ins_php = do
    getP >>= push . (.|. 0x30)
    addPC 1
    tick 3

{- INLINE ins_plp -}
ins_plp :: Emu6502 m => m ()
ins_plp = do
    pull >>= putP
    addPC 1
    tick 4

{- INLINE ins_pla -}
ins_pla :: Emu6502 m => m ()
ins_pla = do
    v0 <- pull
    putA v0
    setN v0
    setZ v0
    addPC 1
    tick 4

{- INLINE nmi -}
nmi :: Emu6502 m => Bool -> m ()
nmi sw = do
    p0 <- getPC
    push $ i8 (p0 `shift` (-8))
    push $ i8 p0
    putB sw
    getP >>= push . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk XXX
    tick 7

{- INLINE ins_rti -}
ins_rti :: Emu6502 m => m ()
ins_rti = do
    pull >>= putP
    make16 <$> pull <*> pull >>= putPC
    tick 6

-- BBC stuff XXX
{- INLINE ins_jsr -}
ins_jsr :: Emu6502 m => m ()
ins_jsr = do
    p0 <- getPC
    getPC >>= read16 . (+1) >>= putPC
    let p2 = p0+2
    push $ i8 (p2 `shift` (-8))
    push $ i8 p2
    tick 6

{- INLINE ins_rts -}
ins_rts :: Emu6502 m => m ()
ins_rts = do
    make16 <$> pull <*> pull >>= putPC . (+1)
    tick 6

{- INLINE step -}
step :: Emu6502 m => m ()
step = do
    debugStrLn 9 "------"
    dumpState
    p0 <- getPC
    --if p0 == 0x3781 then debug .= True else return () -- XXX
    if p0 == 0x400 then liftIO $ putStrLn "Started!!!" else return ()
    if p0 == 0x3770 then liftIO $ putStrLn "Passed!!!" else return ()
    debugStrLn 9 $ "pc = " ++ showHex p0 ""
    i <- readMemory p0
    debugStrLn 9 $ "instruction = " ++ showHex i ""
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
            debugStrLn 9 $ "cc = " ++ show cc
            case cc of
                0b00 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of
                        0b001 -> ins_bit bbb
                        0b010 -> ins_jmp
                        0b011 -> ins_jmp_indirect
                        0b100 -> ins_sty bbb
                        0b101 -> ins_ldy bbb
                        0b110 -> ins_cpy bbb
                        0b111 -> ins_cpx bbb

                        otherwise -> illegal i

                0b01 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> ins_ora bbb
                        0b001 -> ins_and bbb
                        0b010 -> ins_xor bbb
                        0b011 -> ins_adc bbb
                        0b100 -> ins_sta bbb
                        0b101 -> ins_lda bbb
                        0b110 -> ins_cmp bbb
                        0b111 -> ins_sbc bbb

                        otherwise -> illegal i
                0b10 -> do
                    let aaa = (i `shift` (-5)) .&. 0b111
                    let bbb = (i `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> ins_asl bbb
                        0b001 -> ins_rol bbb
                        0b010 -> ins_lsr bbb
                        0b011 -> ins_ror bbb
                        0b100 -> ins_stx bbb
                        0b101 -> ins_ldx bbb
                        0b110 -> ins_dec bbb
                        0b111 -> ins_inc bbb

                otherwise -> illegal i
    dumpState
    return ()

newtype OReg = OReg Word16 deriving (Ord, Ix, Eq, Num)
newtype IReg = IReg Word16 deriving (Ord, Ix, Eq, Num)

nusiz0, nusiz1, colup0, colup1, pf0, pf1, pf2, enam0, enam1, enabl, hmp0, hmp1, hmm0, hmm1, hmbl :: OReg
grp0, grp1, refp0, refp1, colupf, colubk, ctrlpf, resmp0, resmp1, vdelp0, vdelp1, vdelbl :: OReg
vsync = 0x00
nusiz0 = 0x04
nusiz1 = 0x05
colup0 = 0x06
colup1 = 0x07
colupf = 0x08
colubk = 0x09
ctrlpf = 0x0a
refp0 = 0x0b
refp1 = 0x0c
pf0 = 0x0d
pf1 = 0x0e
pf2 = 0x0f
grp0 = 0x1b
grp1 = 0x1c
enam0 = 0x1d
enam1 = 0x1e
enabl = 0x1f
hmp0 = 0x20
hmp1 = 0x21
hmm0 = 0x22
hmm1 = 0x23
hmbl = 0x24
vdelp0 = 0x25
vdelp1 = 0x26
vdelbl = 0x27
resmp0 = 0x28
resmp1 = 0x29

cxm0p, cxm1p, cxp0fb, cxp1fb, cxm0fb, cxm1fb, cxblpf, cxppmm, inpt4, inpt5 :: IReg
cxm0p = 0x00
cxm1p = 0x01
cxp0fb = 0x02
cxp1fb = 0x03
cxm0fb = 0x04
cxm1fb = 0x05
cxblpf = 0x06
cxppmm = 0x07
inpt4 = 0x0c
inpt5 = 0x0d

data Stella = Stella {
     _oregisters :: IOUArray OReg Word8,
     _iregisters :: IOUArray IReg Word8,
    _stellaClock :: !Int64,
    _hpos :: !CInt,
    _vpos :: !CInt,
    _tvSurface :: !Surface,
    _vblank :: !Word8,
    _ppos0 :: !CInt,
    _ppos1 :: !CInt,
    _swcha :: !Word8,
    _swchb :: !Word8,
    _mpos0 :: !CInt,
    _mpos1 :: !CInt,
    _bpos :: !CInt,
    _intim :: !Word8,
    _subtimer :: !CInt,
    _interval :: !CInt,
    _stellaDebug :: !Int,
    _lastClock :: !Int64,
    _xbreak :: !Int32,
    _ybreak :: !Int32
}

$(makeLenses ''Stella)

{- INLINE stellaDebugStr -}
stellaDebugStr n str = do
    d <- use stellaDebug
    if n <= d
        then do
            before <- use lastClock
            now <- use stellaClock
            liftIO $ putStr $ show now ++ " +" ++ show (now-before) ++ ": " ++ str
            lastClock .= now
        else return ()

{- INLINE stellaDebugStrLn -}
stellaDebugStrLn n str = do
    d <- use stellaDebug
    if n <= d
        then do
            before <- use lastClock
            now <- use stellaClock
            liftIO $ putStrLn $ show now ++ " +" ++ show (now-before) ++ ": " ++ str
            lastClock .= now
        else return ()

{- INLINE putORegister -}
putORegister :: (MonadIO m, MonadState Stella m) => OReg -> Word8 -> m ()
putORegister i v = do
    r <- use oregisters
    liftIO $ writeArray r i v

{- INLINE getORegister -}
getORegister :: (MonadIO m, MonadState Stella m) => OReg -> m Word8
getORegister i = do
    r <- use oregisters
    liftIO $ readArray r i

{- INLINE putIRegister -}
putIRegister :: (MonadIO m, MonadState Stella m) => IReg -> Word8 -> m ()
putIRegister i v = do
    r <- use iregisters
    liftIO $ writeArray r i v

{- INLINE getIRegister -}
getIRegister :: (MonadIO m, MonadState Stella m) => IReg -> m Word8
getIRegister i = do
    r <- use iregisters
    liftIO $ readArray r i

{- INLINE orIRegister -}
orIRegister :: (MonadIO m, MonadState Stella m) => IReg -> Word8 -> m ()
orIRegister i v = do
    r <- use iregisters
    liftIO $ readArray r i >>= writeArray r i . (v .|.)

inBinary :: (Bits a) => Int -> a -> String
inBinary 0 x = ""
inBinary n x = inBinary (n-1) (x `shift` (-1)) ++ if testBit x 0 then "1" else "0"

explainNusiz :: Word8 -> String
explainNusiz nusiz =
    case nusiz .&. 0b111 of
        0b000 -> "one copy"
        0b001 -> "two copies - close"
        0b010 -> "two copies - med"
        0b011 -> "three copies - close"
        0b100 -> "two copies - wide"
        0b101 -> "double size player"
        0b110 -> "3 copies medium"
        0b111 -> "quad sized player"

dumpStella :: (MonadIO m, MonadState Stella m) => m ()
dumpStella = do
    hpos' <- use hpos
    vpos' <- use vpos
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    grp0' <- getORegister grp0
    grp1' <- getORegister grp1
    liftIO $ putStrLn $ "GRP0 = " ++ showHex grp0' "" ++ "(" ++ inBinary 8 grp0' ++ ")"
    liftIO $ putStrLn $ "GRP1 = " ++ showHex grp1' "" ++ "(" ++ inBinary 8 grp1' ++ ")"
    pf0' <- getORegister pf0
    pf1' <- getORegister pf1
    pf2' <- getORegister pf2
    liftIO $ putStrLn $ "PF = " ++ reverse (inBinary 4 (pf0' `shift` (-4)))
                                ++ inBinary 8 pf1'
                                ++ reverse (inBinary 8 pf2')
    nusiz0' <- getORegister nusiz0
    nusiz1' <- getORegister nusiz1
    liftIO $ putStrLn $ "NUSIZ0 = " ++ showHex nusiz0' "" ++ "(" ++ explainNusiz nusiz0' ++
                        ") NUSIZ1 = " ++ showHex nusiz1' "" ++ "(" ++ explainNusiz nusiz1' ++ ")"
    vdelp0' <- getORegister vdelp0
    vdelp1' <- getORegister vdelp1
    vdelbl' <- getORegister vdelbl
    liftIO $ putStrLn $ "VDELP0 = " ++ showHex vdelp0' "" ++ " " ++
                        "VDELP1 = " ++ showHex vdelp1' "" ++ " " ++
                        "VDELBL = " ++ showHex vdelbl' ""
    

{- INLINE playfield -}
playfield :: (MonadIO m, MonadState Stella m) => Int -> m Bool
playfield i | i >= 0 && i < 4 = do
                pf0' <- getORegister pf0
                return $ testBit pf0' (i+4)
            | i >=4 && i < 12 = do
                pf1' <- getORegister pf1
                return $ testBit pf1' (11-i)
            | i >= 12 && i < 20 = do
                pf2' <- getORegister pf2
                return $ testBit pf2' (i-12)
playfield i | i >= 20 && i < 40 = do
                ctrlpf' <- getORegister ctrlpf
                playfield $ if testBit ctrlpf' 0 then 39-i else i-20

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True x = x
flipIf False x = 7-x

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Word8 -> CInt -> Word8 -> Bool
stretchPlayer reflect sizeCopies o bitmap =
    case sizeCopies of
        0b000 -> -- one copy
            if o >= 0 && o < 8
                then testBit bitmap (flipIf reflect $ fromIntegral o)
                else False
        0b001 -> -- two copies close
            if o >= 0 && o < 8 || o >= 16 && o < 24
                then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                else False
        0b010 -> -- two copies - med
            if o >= 0 && o < 8 || o >= 32 && o < 40
                then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                else False
        0b011 -> -- three copies close
            if o >= 0 && o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40
                then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                else False
        0b100 -> -- two copies wide
            if o >= 0 && o < 8 || o >= 64 && o < 72
                then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                else False
        0b101 -> -- double size player
            if o >= 0 && o < 16
                then testBit bitmap (flipIf reflect $ fromIntegral ((o `shift` (-1)) .&. 7))
                else False
        0b110 -> -- three copies medium
            if o >= 0 && o < 8 || o >= 32 && o < 40 || o >= 64 && o < 72
                then testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
                else False
        0b111 -> -- quad sized player
            if o >= 0 && o < 32
                then testBit bitmap (flipIf reflect $ (fromIntegral ((o `shift` (-2)) .&. 7)))
                else False

-- Stella programmer's guide p.40
{- INLINE player0 -}
player0 :: (MonadIO m, MonadState Stella m) => m Bool
player0 = do
    hpos' <- use hpos
    ppos0' <- use ppos0
    let o = hpos'-ppos0' :: CInt
    sizeCopies <- (0b111 .&.) <$> getORegister nusiz0
    grp0' <- getORegister grp0
    refp0' <- getORegister refp0
    let reflected = testBit refp0' 3
    return $ stretchPlayer reflected sizeCopies o grp0'

{- INLINE player1 -}
player1 :: (MonadIO m, MonadState Stella m) => m Bool
player1 = do
    hpos' <- use hpos
    ppos1' <- use ppos1
    let o = hpos'-ppos1' :: CInt
    sizeCopies <- (0b111 .&.) <$> getORegister nusiz1
    grp1' <- getORegister grp1
    refp1' <- getORegister refp1
    return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

-- Stella programmer's guide p.22
{- INLINE missile0 -}
missile0 :: (MonadIO m, MonadState Stella m) => m Bool
missile0 = do
    enam0' <- getORegister enam0
    resmp0' <- getORegister resmp0
    if testBit enam0' 1
        then do
            hpos' <- use hpos
            when (testBit resmp0' 1) $ use hpos >>= (mpos0 .=)
            mpos0' <- use mpos0
            let o = hpos'-mpos0' :: CInt
            nusiz0' <- getORegister nusiz0
            let missileSize = 1 `shift` (fromIntegral ((nusiz0' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < missileSize
        else return False

{- INLINE missile1 -}
missile1 :: (MonadIO m, MonadState Stella m) => m Bool
missile1 = do
    enam1' <- getORegister enam1
    resmp1' <- getORegister resmp1
    if testBit enam1' 1
        then do
            hpos' <- use hpos 
            when (testBit resmp1' 1) $ use hpos >>= (mpos1 .=) -- XXX may need to do this on resmp
            mpos1' <- use mpos1
            let o = hpos'-mpos1' :: CInt
            nusiz1' <- getORegister nusiz1
            let missileSize = 1 `shift` (fromIntegral ((nusiz1' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < missileSize
        else return False

{- INLINE ball -}
ball :: (MonadIO m, MonadState Stella m) => m Bool
ball = do
    enabl' <- getORegister enabl
    if (enabl' .&. 0b10) /= 0
        then do
            o <- (-) <$> use hpos <*> use bpos
            ctrlpf' <- getORegister ctrlpf
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
            return $ o >= 0 && o < ballSize
        else return False

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (160, 192)

{- INLINE clockMove -}
clockMove :: Word8 -> CInt
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

{- INLINE stellaHmclr -}
stellaHmclr :: (MonadIO m, MonadState Stella m) => m ()
stellaHmclr = do
    putORegister hmp0 0
    putORegister hmp1 0
    putORegister hmm0 0
    putORegister hmm1 0
    putORegister hmbl 0

{- INLINE stellaCxclr -}
stellaCxclr :: (MonadIO m, MonadState Stella m) => m ()
stellaCxclr = do
    putIRegister cxm0p 0
    putIRegister cxm1p 0
    putIRegister cxm0fb 0
    putIRegister cxm1fb 0
    putIRegister cxp0fb 0
    putIRegister cxp1fb 0
    putIRegister cxblpf 0
    putIRegister cxppmm 0

{-# INLINE wrap160 #-}
wrap160 :: CInt -> CInt
wrap160 i | i>=picx && i < picx+160 = i
          | i < picx = wrap160 (i+160)
          | i >= picx+160 = wrap160 (i-160)

{- INLINE stellaHmove -}
stellaHmove :: (MonadIO m, MonadState Stella m) => m ()
stellaHmove = do
    poffset0 <- getORegister hmp0
    ppos0' <- use ppos0
    ppos0 .= wrap160 (ppos0'-clockMove poffset0)

    poffset1 <- getORegister hmp1
    ppos1 -= clockMove poffset1

    moffset0 <- getORegister hmm0
    mpos0 -= clockMove moffset0

    moffset1 <- getORegister hmm1
    mpos1' <- use mpos1
    mpos1 .= wrap160 (mpos1'-clockMove moffset1) -- XXX do rest

    boffset <- getORegister hmbl
    bpos -= clockMove boffset

{- INLINE stellaResmp0 -}
stellaResmp0 :: (MonadIO m, MonadState Stella m) => m ()
stellaResmp0 = use ppos0 >>= (mpos0 .=) -- XXX

{- INLINE stellaResmp1 -}
stellaResmp1 :: (MonadIO m, MonadState Stella m) => m ()
stellaResmp1 = use ppos1 >>= (mpos1 .=) -- XXX

{- INLINE stellaWsync -}
stellaWsync :: (MonadIO m, MonadState Stella m) => m ()
stellaWsync = do
    hpos' <- use hpos
    stellaTick (228-fromIntegral hpos')
    stellaDebugStrLn 0 $ "WSYNC (post)"

-- http://atariage.com/forums/topic/107527-atari-2600-vsyncvblank/

{- INLINE stellaVsync -}
stellaVsync :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVsync v = do
    stellaDebugStrLn 0 $ "VSYNC " ++ showHex v ""
    oldv <- getORegister vsync
    when (testBit oldv 1 && not (testBit v 1)) $ do
            hpos .= 0
            vpos .= 0
    putORegister vsync v

{- INLINE stellaVblank -}
stellaVblank :: (MonadIO m, MonadState Stella m) => Word8 -> m ()
stellaVblank v = do
    stellaDebugStrLn 0 $ "VBLANK " ++ showHex v ""
    vold <- use vblank
    -- Set latches for INPT4 and INPT5
    when (testBit v 6) $ do
        i <- getIRegister inpt4 -- XXX write modifyIRegister
        putIRegister inpt4 (setBit i 7)
        i <- getIRegister inpt5
        putIRegister inpt5 (setBit i 7)

    --liftIO $ putStrLn $ show vold ++ " -> " ++ show v
    {-
    if (vold .&. 0b00000010 /= 0) && (v .&. 0b00000010 == 0)
        then do
            --liftIO $ print "VBLANK"
            hpos .= 0
            vpos .= 0
        else return ()
    -}
    vblank .= v

-- player0

picy :: CInt
picy = 40
picx :: CInt
picx = 68

data Pixel = Pixel { plogic :: !Bool, pcolor :: !Word8 }

instance Monoid Pixel where
    {- INLINE mappend -}
    mempty = Pixel False 0
    _ `mappend` pixel@(Pixel True _) = pixel
    pixel `mappend` (Pixel False _) = pixel

bit :: Int -> Bool -> Word8
bit n t = if t then 1 `shift` n else 0

{- INLINE compositeAndCollide -}
compositeAndCollide :: (MonadIO m, MonadState Stella m) => CInt -> m Word8
compositeAndCollide x = do
    ctrlpf' <- getORegister ctrlpf
    colupf' <- getORegister colupf
    colup0' <- getORegister colup0
    colup1' <- getORegister colup1
    let playfieldColour = if testBit ctrlpf' 1
            then if x < 80
                then colup0'
                else colup1'
            else colupf'

    -- Assemble colours
    pbackground <- Pixel True <$> getORegister colubk
    pplayfield <- Pixel <$> playfield (fromIntegral $ x `div` 4) <*> return playfieldColour
    pplayer0 <- Pixel <$> player0 <*> return colup0'
    pplayer1 <- Pixel <$> player1 <*> return colup1'
    pmissile0 <- Pixel <$> missile0 <*> return colup0'
    pmissile1 <- Pixel <$> missile1 <*> return colup1'
    pball <- Pixel <$> ball <*> getORegister colupf

    orIRegister cxm0p $ bit 7 (plogic pmissile0 && plogic pplayer1) .|.
                        bit 6 (plogic pmissile0 && plogic pplayer0)
    orIRegister cxm1p $ bit 7 (plogic pmissile1 && plogic pplayer0) .|.
                        bit 6 (plogic pmissile1 && plogic pplayer1)
    orIRegister cxp0fb $ bit 7 (plogic pplayer0 && plogic pplayfield) .|.
                         bit 6 (plogic pplayer0 && plogic pball)
    orIRegister cxp1fb $ bit 7 (plogic pplayer1 && plogic pplayfield) .|.
                         bit 6 (plogic pplayer1 && plogic pball)
    orIRegister cxm0fb $ bit 7 (plogic pmissile0 && plogic pplayfield) .|.
                         bit 6 (plogic pmissile0 && plogic pball)
    orIRegister cxm1fb $ bit 7 (plogic pmissile1 && plogic pplayfield) .|.
                         bit 6 (plogic pmissile1 && plogic pball)
    orIRegister cxblpf $ bit 7 (plogic pball && plogic pplayfield)
    orIRegister cxppmm $ bit 7 (plogic pplayer0 && plogic pplayer1) .|.
                         bit 6 (plogic pmissile0 && plogic pmissile1)
    
    -- Get ordering priority
    let Pixel _ final = pbackground `mappend`
                        if testBit ctrlpf' 2
                            then mconcat [pplayer1, pmissile1, pplayer0, pmissile0, pplayfield, pball]
                            else mconcat [pball, pplayfield, pplayer1, pmissile1, pplayer0, pmissile0]
    return final

stellaTick :: (MonadIO m, MonadState Stella m) => Int -> m ()
stellaTick 0 = return ()
stellaTick n = do
    xbreak' <- use xbreak
    ybreak' <- use ybreak
    hpos' <- use hpos
    vpos' <- use vpos
    when (hpos' == fromIntegral xbreak' && vpos' == fromIntegral ybreak') $ do
        dumpStella
        xbreak .= (-1)
        ybreak .= (-1)

    -- Interval timer
    x <- use intim
    y <- use interval
    stellaDebugStrLn 0 $ "clock = " ++ show x
    stellaClock += 1
    subtimer' <- use subtimer
    let subtimer'' = subtimer'-1
    subtimer .= subtimer''
    when (subtimer' == 0) $ do
        interval' <- use interval
        subtimer .= 3*interval'-1
        intim' <- use intim
        let intim'' = intim'-1
        intim .= intim''
        when (intim' == 0) $ do
            subtimer .= 3*1-1
            interval .= 1
    
    -- Display
    when (vpos' >= picy && vpos' < picy+192 && hpos' >= picx) $ do
        surface <- use tvSurface
        ptr <- liftIO $ surfacePixels surface
        let ptr' = castPtr ptr :: Ptr Word32
        let x = hpos'-picx
        let y = vpos'-picy
        let i = screenWidth*y+x

        final <- compositeAndCollide x

        liftIO $ pokeElemOff ptr' (fromIntegral i) (lut!(final `shift` (-1)))
    hpos += 1
    hpos' <- use hpos
    when (hpos' >= picx+160) $ do
        hpos .= 0
        vpos += 1
        vpos' <- use vpos
        when (vpos' >= picy+192) $ vpos .= 0
    stellaTick (n-1)

renderFrame :: (MonadIO m, MonadState Stella m) => m ()
renderFrame = do
    surface <- use tvSurface
    ptr <- liftIO $ surfacePixels surface
    let ptr' = castPtr ptr :: Ptr Word32
    liftIO $ lockSurface surface
    forM_ [0..screenHeight-1] $ \row -> do
        forM_ [0..screenWidth-1] $ \col -> do
            let i = screenWidth*row+col
            liftIO $ pokeElemOff ptr' (fromIntegral i) (fromIntegral col)
            return ()
    liftIO $ unlockSurface surface

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

makeLenses ''Registers

{- INLINE flagC -}
flagC :: Lens' Registers Bool
flagC = p . bitAt 0

{- INLINE flagZ -}
flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

{- INLINE flagI -}
flagI :: Lens' Registers Bool
flagI = p . bitAt 2

{- INLINE flagD -}
flagD :: Lens' Registers Bool
flagD = p . bitAt 3

{- INLINE flagB -}
flagB :: Lens' Registers Bool
flagB = p . bitAt 4

{- INLINE flagV -}
flagV :: Lens' Registers Bool
flagV = p . bitAt 6

{- INLINE flagN -}
flagN :: Lens' Registers Bool
flagN = p . bitAt 7

data StateAtari = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int,
    _regs :: !Registers,
    _debug :: !Int,
    _stella :: Stella
}

makeLenses ''StateAtari

newtype MonadAtari a = M { unM :: StateT StateAtari IO a }
    deriving (Functor, Applicative, Monad, MonadState StateAtari, MonadIO)

--  XXX Do this! If reset occurs during horizontal blank, the object will appear at the left side of the television screen

{- INLINE setBreak -}
setBreak :: (MonadIO m, MonadState Stella m) =>
               Int32 -> Int32 -> m ()
setBreak x y = do
    xbreak .= x+fromIntegral picx
    ybreak .= y+fromIntegral picy

{- INLINE usingStella -}
usingStella :: MonadState StateAtari m =>
               StateT Stella m a -> m a
usingStella m = do
    stella' <- use stella
    (a, stella'') <- flip runStateT stella' m
    stella .= stella''
    return a

{- INLINE writeStella -}
writeStella :: (MonadIO m, MonadState Stella m) =>
               Word16 -> Word8 -> m ()
writeStella addr v = 
    case addr of
       0x00 -> stellaVsync v             -- VSYNC
       0x01 -> stellaVblank v            -- VBLANK
       0x02 -> stellaWsync               -- WSYNC
       0x04 -> putORegister nusiz0 v        -- NUSIZ0
       0x05 -> putORegister nusiz1 v        -- NUSIZ1
       0x06 -> putORegister colup0 v               -- COLUP0
       0x07 -> putORegister colup1 v               -- COLUP1
       0x08 -> putORegister colupf v               -- COLUPF
       0x09 -> putORegister colubk v               -- COLUBK
       0x0a -> putORegister ctrlpf v               -- COLUPF
       0x0b -> putORegister refp0 v               -- REFP0
       0x0c -> putORegister refp1 v               -- REFP1
       0x0d -> putORegister pf0 v                  -- PF0
       0x0e -> putORegister pf1 v                  -- PF1
       0x0f -> putORegister pf2 v                  -- PF2
       0x10 -> use hpos >>= (ppos0 .=)   -- RESP0
       0x11 -> use hpos >>= (ppos1 .=)   -- RESP1
       0x12 -> use hpos >>= (mpos0 .=)   -- RESM0
       0x13 -> use hpos >>= (mpos1 .=)   -- RESM1
       0x14 -> use hpos >>= (bpos .=)    -- RESBL
       0x1b -> putORegister grp0 v                 -- GRP0
       0x1c -> putORegister grp1 v                 -- GRP1
       0x1d -> putORegister enam0 v                -- ENAM0
       0x1e -> putORegister enam1 v                -- ENAM1
       0x1f -> putORegister enabl v                -- ENABL
       0x20 -> putORegister hmp0 v                 -- HMP0
       0x21 -> putORegister hmp1 v                 -- HMP1
       0x22 -> putORegister hmm0 v                 -- HMM0
       0x23 -> putORegister hmm1 v                 -- HMM1
       0x24 -> putORegister hmbl v                 -- HMBL
       0x25 -> putORegister vdelp0 v
       0x26 -> putORegister vdelp1 v
       0x27 -> putORegister vdelbl v
       0x28 -> putORegister resmp0 v
       0x29 -> putORegister resmp1 v
       0x2a -> stellaHmove               -- HMOVE
       0x2b -> stellaHmclr               -- HMCLR
       0x2c -> stellaCxclr               -- CXCLR
       0x294 -> do                       -- TIM1T
        interval .= 1
        subtimer .= 1*3-1
        intim .= v
       0x295 -> do                       -- TIM8T
        interval .= 8
        subtimer .= 8*3-1
        intim .= v
       0x296 -> do                       -- TIM64T
        interval .= 64
        subtimer .= 64*3-1
        intim .= v
       0x297 -> do                       -- TIM1024T
        interval .= 1024
        subtimer .= 1024*3-1
        intim .= v
       otherwise -> return () -- liftIO $ putStrLn $ "writing TIA 0x" ++ showHex addr ""

{- INLINE readStella -}
readStella :: (MonadIO m, MonadState Stella m) =>
              Word16 -> m Word8
readStella addr = 
    case addr of
        0x00 -> getIRegister cxm0p
        0x01 -> getIRegister cxm1p
        0x02 -> getIRegister cxp0fb
        0x03 -> getIRegister cxp1fb
        0x04 -> getIRegister cxm0fb
        0x05 -> getIRegister cxm1fb
        0x06 -> getIRegister cxblpf
        0x07 -> getIRegister cxppmm
        0x0c -> getIRegister inpt4
        0x10 -> getIRegister cxm0p
        0x11 -> getIRegister cxm1p
        0x12 -> getIRegister cxp0fb
        0x13 -> getIRegister cxp1fb
        0x14 -> getIRegister cxm0fb
        0x15 -> getIRegister cxm1fb
        0x16 -> getIRegister cxblpf
        0x17 -> getIRegister cxppmm
        0x1c -> getIRegister inpt4
        0x20 -> getIRegister cxm0p
        0x21 -> getIRegister cxm1p
        0x22 -> getIRegister cxp0fb
        0x23 -> getIRegister cxp1fb
        0x24 -> getIRegister cxm0fb
        0x25 -> getIRegister cxm1fb
        0x26 -> getIRegister cxblpf
        0x27 -> getIRegister cxppmm
        0x2c -> getIRegister inpt4
        0x30 -> getIRegister cxm0p
        0x31 -> getIRegister cxm1p
        0x32 -> getIRegister cxp0fb
        0x33 -> getIRegister cxp1fb
        0x34 -> getIRegister cxm0fb
        0x35 -> getIRegister cxm1fb
        0x36 -> getIRegister cxblpf
        0x37 -> getIRegister cxppmm
        0x3c -> getIRegister inpt4
        0x280 -> use swcha
        0x282 -> use swchb
        0x284 -> use intim
        otherwise -> return 0 -- (liftIO $ putStrLn $ "reading TIA 0x" ++ showHex addr "") >> return 0

-- http://www.qotile.net/minidig/docs/2600_mem_map.txt

--
-- Decision tree for type of memory
--
-- testBit a 12
-- True -> ROM
-- False -> testBit a 7
--          False -> TIA
--          True -> testBit a 9
--                  True -> RIOT
--                  False -> RAM
{-# INLINE isTIA #-}
isTIA :: Word16 -> Bool
isTIA a = not (testBit a 7) && not (testBit a 12)

{-# INLINE isRAM #-}
isRAM :: Word16 -> Bool
isRAM a = testBit a 7 && not (testBit a 9) && not (testBit a 12)

{-# INLINE isRIOT #-}
isRIOT :: Word16 -> Bool
isRIOT a = testBit a 7 && testBit a 9 && not (testBit a 12)

{-# INLINE isROM #-}
isROM :: Word16 -> Bool
isROM a = testBit a 12

instance Emu6502 MonadAtari where
    {- INLINE readMemory -}
    readMemory addr' =
        let addr = addr' .&. 0b1111111111111 in -- 6507
            if isTIA addr
                then usingStella $ readStella (addr .&. 0x3f)
                else if isRAM addr
                        then do
                            m <- use mem
                            liftIO $ readArray m (fromIntegral addr .&. 0xff)
                        else if isRIOT addr
                            then usingStella $ readStella (0x280+(addr .&. 0x1f))
                            else if addr >= 0x1000
                                then do
                                    m <- use mem
                                    liftIO $ readArray m (fromIntegral addr)
                                else error $ "Mystery read from " ++ showHex addr ""


    {- INLINE writeMemory -}
    writeMemory addr' v =
        let addr = addr' .&. 0b1111111111111 in -- 6507
            if isTIA addr
                then usingStella $ writeStella (addr .&. 0x3f) v
                else if isRAM addr
                        then do
                            m <- use mem
                            liftIO $ writeArray m (fromIntegral addr .&. 0xff) v
                        else if isRIOT addr
                                then usingStella $ writeStella (0x280+(addr .&. 0x1f)) v
                                else if addr >= 0x1000
                                    then do
                                        m <- use mem
                                        liftIO $ writeArray m (fromIntegral addr) v
                                    else error $ "Mystery write to " ++ showHex addr ""

    {- INLINE getPC -}
    getPC = use (regs . pc)
    {- INLINE tick -}
    tick n = do
        clock += n
        usingStella $ stellaTick (3*n)
    {- INLINE putC -}
    putC b = regs . flagC .= b
    {- INLINE getC -}
    getC = use (regs . flagC)
    {- INLINE putZ -}
    putZ b = regs . flagZ .= b
    {- INLINE getZ -}
    getZ = use (regs . flagZ)
    {- INLINE putI -}
    putI b = regs . flagI .= b
    {- INLINE getI -}
    getI = use (regs . flagI)
    {- INLINE putD -}
    putD b = regs . flagD .= b
    {- INLINE getD -}
    getD = use (regs . flagD)
    {- INLINE putB -}
    putB b = regs . flagB .= b
    {- INLINE getB -}
    getB = use (regs . flagB)
    {- INLINE putV -}
    putV b = regs . flagV .= b
    {- INLINE getV -}
    getV = use (regs . flagV)
    {- INLINE putN -}
    putN b = regs . flagN .= b
    {- INLINE getN -}
    getN = use (regs . flagN)
    {- INLINE getA -}
    getA = use (regs . a)
    {- INLINE putA -}
    putA r = regs . a .= r
    {- INLINE getS -}
    getS = use (regs . s)
    {- INLINE putS -}
    putS r = regs . s .= r
    {- INLINE getX -}
    getX = use (regs . x)
    {- INLINE putX -}
    putX r = regs . x .= r
    {- INLINE getP -}
    getP = use (regs . p)
    {- INLINE putP -}
    putP r = regs . p .= r
    {- INLINE getY -}
    getY = use (regs . y)
    {- INLINE putY -}
    putY r = regs . y .= r
    {- INLINE putPC -}
    putPC r = regs . pc .= r
    {- INLINE addPC -}
    addPC n = regs . pc += fromIntegral n

    {- INLINE debugStr -}
    debugStr n str = do
        d <- use debug
        if n <= d
            then liftIO $ putStr str
            else return ()

    {- INLINE debugStrLn -}
    debugStrLn n str = do
        d <- use debug
        if n <= d
            then liftIO $ putStrLn str
            else return ()

    {- INLINE illegal -}
    illegal i = error $ "Illegal opcode 0x" ++ showHex i ""


data Args = Args { file :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "adventure.bin" }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

scale :: CInt
scale = 4

{- INLINE isPressed -}
isPressed :: InputMotion -> Bool
isPressed Pressed = True
isPressed Released = False

main :: IO ()
main = do
  args <- cmdArgs clargs
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Stellarator" SDL.defaultWindow { SDL.windowInitialSize = V2 (scale*screenWidth) (scale*screenHeight) }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  helloWorld <- createRGBSurface (V2 screenWidth screenHeight) RGB888

  memory <- newArray (0, 0x2000) 0 :: IO (IOUArray Int Word8)
  readBinary memory (file args) 0x1000
  pclo <- readArray memory 0x1ffc
  pchi <- readArray memory 0x1ffd
  let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)

  oregs <- newArray (0, 0x3f) 0
  iregs <- newArray (0, 0x0d) 0
  let stella = Stella {
      _oregisters = oregs,
      _iregisters = iregs,
      _stellaClock = 0,
      _hpos = 0,
      _vpos = 0,
      _tvSurface = helloWorld,
      _vblank = 0,
      _ppos0 = 9999,
      _ppos1 = 9999,
      _swcha = 0xff,
      _swchb = 0b00001011,
      _mpos0 = 0, _mpos1 = 0,
      _bpos = 0,
      _intim = 0,
      _subtimer = 0,
      _interval = 0,
      _lastClock = 0,
      _stellaDebug = -1,
      _xbreak = -1,
      _ybreak = -1
  }
  let state = S { _mem = memory,  _clock = 0, _regs = R initialPC 0 0 0 0 0xff,
                   _debug = 8,
                   _stella = stella}

  let loopUntil n = do
        stellaClock' <- usingStella $ use stellaClock
        when (stellaClock' < n) $ do
            unM step
            loopUntil n

  --SDL.setHintWithPriority SDL.NormalPriority SDL.HintRenderVSync SDL.EnableVSync
  -- https://hackage.haskell.org/package/sdl2-2.1.3

  let loop = do
        events <- liftIO $ SDL.pollEvents

        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        forM_ events $ \event ->
            case eventPayload event of
                MouseButtonEvent (MouseButtonEventData win Pressed device ButtonLeft clicks pos) -> do
                        liftIO $ print pos
                        let P (V2 x y) = pos
                        usingStella $ setBreak (x `div` fromIntegral scale) (y `div` fromIntegral scale)
                MouseMotionEvent (MouseMotionEventData win device [ButtonLeft] pos rel) -> do
                        liftIO $ print pos
                        let P (V2 x y) = pos
                        usingStella $ setBreak (x `div` fromIntegral scale) (y `div` fromIntegral scale)
                KeyboardEvent (KeyboardEventData win motion rep sym) -> do
                    let pressed = isPressed motion
                    --liftIO $ print sym
                    case keysymScancode sym of
                        SDL.ScancodeUp -> usingStella $ swcha . bitAt 4 .= not pressed
                        SDL.ScancodeDown -> usingStella $ swcha . bitAt 5 .= not pressed
                        SDL.ScancodeLeft -> usingStella $ swcha . bitAt 6 .= not pressed
                        SDL.ScancodeRight -> usingStella $ swcha . bitAt 7 .= not pressed
                        SDL.ScancodeC -> usingStella $ swchb . bitAt 1 .= not pressed
                        SDL.ScancodeV -> usingStella $ swchb . bitAt 0 .= not pressed
                        SDL.ScancodeSpace -> usingStella $ do
                            latch <- use (vblank . bitAt 6)
                            case (latch, pressed) of
                                (False, _) -> do
                                    inpt4' <- getIRegister inpt4
                                    putIRegister inpt4 ((clearBit inpt4' 7) .|. bit 7 (not pressed))
                                (True, False) -> return ()
                                (True, True) -> do
                                    inpt4' <- getIRegister inpt4
                                    putIRegister inpt4 (clearBit inpt4' 7)
                otherwise -> return ()

        liftIO $ lockSurface screenSurface
        stellaClock' <- usingStella $ use stellaClock
        loopUntil (stellaClock' + 50000)
        liftIO $ unlockSurface screenSurface

        liftIO $ SDL.surfaceBlitScaled helloWorld Nothing screenSurface (Just (Rectangle (P (V2 0 0)) (V2 (screenWidth*scale) (screenHeight*scale))))
        liftIO $ SDL.updateWindowSurface window

        loop

  flip runStateT state $ do
    -- Joystick buttons not pressed
    usingStella $ putIRegister inpt4 0x80
    usingStella $ putIRegister inpt5 0x80
    loop

  SDL.destroyWindow window
  SDL.freeSurface helloWorld
  SDL.quit
