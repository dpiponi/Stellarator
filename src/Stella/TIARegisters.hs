{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Stella.TIARegisters where
--module Stella.TIARegisters(OReg, IReg, fastGetORegister, fastModifyIRegister, fastOrIRegister) where

import Data.Word
import Data.Array.IO
import Data.Array.Base
--import Data.Int
import Data.Bits

newtype TypedIndex t = TO { unTyped :: Int } deriving (Ord, Ix, Eq, Num)

-- Could be Int instead of Word16
-- newtype OReg = OReg Word16 deriving (Ord, Ix, Eq, Num)
-- newtype IReg = IReg Word16 deriving (Ord, Ix, Eq, Num)
-- newtype IntReg = IntReg Int deriving (Ord, Ix, Eq, Num)
-- newtype BoolReg = BoolReg Int deriving (Ord, Ix, Eq, Num)

intim, p, a, x, y, s, oldGrp0, newGrp0, oldGrp1, newGrp1 :: TypedIndex Word8
intim = 0
p = 1
a = 2
x = 3
y = 4
s = 5
oldGrp0 = 6
oldGrp1 = 7
newGrp0 = 8
newGrp1 = 9

nusiz0, nusiz1, colup0, colup1, pf0, pf1, pf2, enam0, enam1, hmp0, hmp1, hmm0, hmm1, hmbl :: TypedIndex Word8
vblank, vsync, refp0, refp1, colupf, colubk, ctrlpf, resmp0, resmp1 :: TypedIndex Word8
vsync = 0x100+0x00
vblank = 0x100+0x01
nusiz0 = 0x100+0x04
nusiz1 = 0x100+0x05
colup0 = 0x100+0x06
colup1 = 0x100+0x07
colupf = 0x100+0x08
colubk = 0x100+0x09
ctrlpf = 0x100+0x0a
refp0 = 0x100+0x0b
refp1 = 0x100+0x0c
pf0 = 0x100+0x0d
pf1 = 0x100+0x0e
pf2 = 0x100+0x0f
enam0 = 0x100+0x1d
enam1 = 0x100+0x1e
hmp0 = 0x100+0x20
hmp1 = 0x100+0x21
hmm0 = 0x100+0x22
hmm1 = 0x100+0x23
hmbl = 0x100+0x24
resmp0 = 0x100+0x28
resmp1 = 0x100+0x29

cxm0p, cxm1p, cxp0fb, cxp1fb, cxm0fb, cxm1fb, cxblpf, cxppmm, inpt4, inpt5 :: TypedIndex Word8
cxm0p = 0x200+0x00
cxm1p = 0x200+0x01
cxp0fb = 0x200+0x02
cxp1fb = 0x200+0x03
cxm0fb = 0x200+0x04
cxm1fb = 0x200+0x05
cxblpf = 0x200+0x06
cxppmm = 0x200+0x07
inpt4 = 0x200+0x0c
inpt5 = 0x200+0x0d

swcha, swchb :: TypedIndex Word8
swcha = 0x300+0x80
swchb = 0x300+0x82

trigger1, delayP0, delayP1, delayBall, oldBall, newBall :: TypedIndex Bool
trigger1 = 0
delayP0 = 1
delayP1 = 2
delayBall = 3
oldBall = 4
newBall = 5

pf :: TypedIndex Word64
pf = 0

hpos, vpos, subtimer, interval, s_ppos0, s_ppos1, s_mpos0, s_mpos1, s_bpos, debug :: TypedIndex Int
hpos = 0
vpos = 1
subtimer = 2
interval = 3
s_ppos0 = 4
s_ppos1 = 5
s_mpos0 = 6
s_mpos1 = 7
s_bpos = 8
debug = 9

pc, bankOffset :: TypedIndex Word16
pc = 0
bankOffset = 1

-- type ORegArray = IOUArray OReg Word8
-- type IRegArray = IOUArray IReg Word8
--type IntRegArray = IOUArray IntReg Int
--type BoolRegArray = IOUArray BoolReg Bool

{-# INLINE ld #-}
ld :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> IO a
ld arr idx = unsafeRead arr (unTyped idx)

{-# INLINE st #-}
st :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> a -> IO ()
st arr idx = unsafeWrite arr (unTyped idx)

{-# INLINE mod #-}
mod :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> (a -> a) -> IO ()
mod arr idx f = do { value <- unsafeRead arr (unTyped idx); unsafeWrite arr (unTyped idx) (f value) }

type Segment a = IOUArray (TypedIndex a) a

{-
{-# INLINE fastGetORegister #-}
fastGetORegister :: IOUArray OReg Word8 -> OReg -> IO Word8
fastGetORegister = readArray

{-# INLINE fastGetIRegister #-}
fastGetIRegister :: IOUArray IReg Word8 -> IReg -> IO Word8
fastGetIRegister = readArray

{-# INLINE fastPutORegister #-}
fastPutORegister :: IOUArray OReg Word8 -> OReg -> Word8 -> IO ()
fastPutORegister = writeArray

{-# INLINE fastPutIRegister #-}
fastPutIRegister :: IOUArray IReg Word8 -> IReg -> Word8 -> IO ()
fastPutIRegister = writeArray

{-# INLINE fastModifyIRegister #-}
fastModifyIRegister :: IOUArray IReg Word8 -> IReg -> (Word8 -> Word8) -> IO ()
fastModifyIRegister r i f = readArray r i >>= writeArray r i . f

{-# INLINE fastOrIRegister #-}
fastOrIRegister :: IOUArray IReg Word8 -> IReg -> Word8 -> IO ()
fastOrIRegister r i v = fastModifyIRegister r i (v .|.)
-}
