module Step where

import Asm
import Emulation
import AcornAtom
import Prelude hiding (and)
import Data.Int
import Control.Monad
import ALU

-- -- Trying not having this inlined {-# IGNOREINLINABLE step #-}
step :: MonadAcorn ()
step = do
    p0 <- getPC
    tick 1
    i <- readMemory p0
    incPC
    case i of
        0x00 -> brk
        0x01 -> ora readIndX
        0x04 -> void $ readZeroPage -- XXX undocumented "DOP" nop
        0x05 -> ora readZeroPage
        0x06 -> asl withZeroPage
        0x08 -> php
        0x09 -> ora readImm
        0x0a -> asl withAcc
        0x0d -> ora readAbs
        0x0e -> asl withAbs
        0x10 -> bra getN False
        0x11 -> ora readIndY
        0x15 -> ora readZeroPageX
        0x16 -> asl withZeroPageX
        0x18 -> set putC False
        0x19 -> ora readAbsY
        0x1d -> ora readAbsX
        0x1e -> asl withAbsX
        0x20 -> jsr
        0x21 -> and readIndX
        0x24 -> bit readZeroPage
        0x25 -> and readZeroPage
        0x26 -> rol withZeroPage
        0x28 -> plp
        0x29 -> and readImm
        0x2a -> rol withAcc
        0x2c -> bit readAbs
        0x2d -> and readAbs
        0x2e -> rol withAbs
        0x30 -> bra getN True
        0x31 -> and readIndY
        0x35 -> and readZeroPageX
        0x36 -> rol withZeroPageX
        0x38 -> set putC True
        0x39 -> and readAbsY
        0x3d -> and readAbsX
        0x3e -> rol withAbsX
        0x40 -> rti
        0x41 -> eor readIndX
        0x45 -> eor readZeroPage
        0x46 -> lsr withZeroPage
        0x48 -> pha
        0x49 -> eor readImm
        0x4a -> lsr withAcc
        0x4c -> jmp
        0x4d -> eor readAbs
        0x4e -> lsr withAbs
        0x50 -> bra getV False
        0x51 -> eor readIndY
        0x55 -> eor readZeroPageX
        0x56 -> lsr withZeroPageX
        0x58 -> set putI False
        0x59 -> eor readAbsY
        0x5d -> eor readAbsX
        0x5e -> lsr withAbsX
        0x60 -> rts
        0x61 -> adc readIndX
        0x65 -> adc readZeroPage
        0x66 -> ror withZeroPage
        0x68 -> pla
        0x69 -> adc readImm
        0x6a -> ror withAcc
        0x6c -> jmp_indirect
        0x6d -> adc readAbs
        0x6e -> ror withAbs
        0x70 -> bra getV True
        0x71 -> adc readIndY
        0x75 -> adc readZeroPageX
        0x76 -> ror withZeroPageX
        0x78 -> set putI True
        0x79 -> adc readAbsY
        0x7d -> adc readAbsX
        0x7e -> ror withAbsX
        0x81 -> sta writeIndX
        0x84 -> sty writeZeroPage
        0x85 -> sta writeZeroPage
        0x86 -> stx writeZeroPage
        0x88 -> der getY putY
        0x8a -> tra getX putA
        0x8c -> sty writeAbs
        0x8d -> sta writeAbs
        0x8e -> stx writeAbs
        0x90 -> bra getC False
        0x91 -> sta writeIndY
        0x94 -> sty writeZeroPageX
        0x95 -> sta writeZeroPageX
        0x96 -> stx writeZeroPageY
        0x98 -> tra getY putA
        0x99 -> sta writeAbsY
        0x9a -> txs
        0x9d -> sta writeAbsX
        0xa0 -> ldy readImm
        0xa1 -> lda readIndX
        0xa2 -> ldx readImm
        0xa4 -> ldy readZeroPage
        0xa5 -> lda readZeroPage
        0xa6 -> ldx readZeroPage
        0xa8 -> tra getA putY
        0xa9 -> lda readImm
        0xaa -> tra getA putX
        0xac -> ldy readAbs
        0xad -> lda readAbs
        0xae -> ldx readAbs
        0xb0 -> bra getC True
        0xb1 -> lda readIndY
        0xb4 -> ldy readZeroPageX
        0xb5 -> lda readZeroPageX
        0xb6 -> ldx readZeroPageY
        0xb8 -> set putV False
        0xb9 -> lda readAbsY
        0xba -> tra getS putX
        0xbc -> ldy readAbsX
        0xbd -> lda readAbsX
        0xbe -> ldx readAbsY
        0xc0 -> cpy readImm
        0xc1 -> cmp readIndX
        0xc4 -> cpy readZeroPage
        0xc5 -> cmp readZeroPage
        0xc6 -> dec withZeroPage
        0xc8 -> inr getY putY
        0xc9 -> cmp readImm
        0xca -> der getX putX
        0xcc -> cpy readAbs
        0xcd -> cmp readAbs
        0xce -> dec withAbs
        0xd0 -> bra getZ False
        0xd1 -> cmp readIndY
        0xd5 -> cmp readZeroPageX
        0xd6 -> dec withZeroPageX
        0xd8 -> set putD False
        0xd9 -> cmp readAbsY
        0xdd -> cmp readAbsX
        0xde -> dec withAbsX
        0xe0 -> cpx readImm
        0xe1 -> sbc readIndX
        0xe4 -> cpx readZeroPage
        0xe5 -> sbc readZeroPage
        0xe6 -> inc withZeroPage
        0xe8 -> inr getX putX
        0xe9 -> sbc readImm
        0xea -> nop
        0xec -> cpx readAbs
        0xed -> sbc readAbs
        0xee -> inc withAbs
        0xf0 -> bra getZ True
        0xf1 -> sbc readIndY
        0xf5 -> sbc readZeroPageX
        0xf6 -> inc withZeroPageX
        0xf8 -> set putD True
        0xf9 -> sbc readAbsY
        0xfd -> sbc readAbsX
        0xfe -> inc withAbsX

        _ -> illegal i

--     dumpState
    return ()

loopUntil :: Int64 -> MonadAcorn ()
loopUntil 0 = return ()
loopUntil n = (pc @-> pcStep) >> step >> loopUntil (n-1)
