{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stella.TIARegisters where
--module Stella.TIARegisters(OReg, IReg, fastGetORegister, fastModifyIRegister, fastOrIRegister) where

import Data.Word
import Data.Array.IO
import Data.Bits

newtype OReg = OReg Word16 deriving (Ord, Ix, Eq, Num)
newtype IReg = IReg Word16 deriving (Ord, Ix, Eq, Num)

nusiz0, nusiz1, colup0, colup1, pf0, pf1, pf2, enam0, enam1, hmp0, hmp1, hmm0, hmm1, hmbl :: OReg
vblank, vsync, refp0, refp1, colupf, colubk, ctrlpf, resmp0, resmp1 :: OReg
vsync = 0x00
vblank = 0x01
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
enam0 = 0x1d
enam1 = 0x1e
hmp0 = 0x20
hmp1 = 0x21
hmm0 = 0x22
hmm1 = 0x23
hmbl = 0x24
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

swcha, swchb :: IReg
swcha = 0x280
swchb = 0x282

{-# INLINE fastGetORegister #-}
fastGetORegister :: IOUArray OReg Word8 -> OReg -> IO Word8
fastGetORegister = readArray

{-# INLINE fastModifyIRegister #-}
fastModifyIRegister :: IOUArray IReg Word8 -> IReg -> (Word8 -> Word8) -> IO ()
fastModifyIRegister r i f = readArray r i >>= writeArray r i . f

{-# INLINE fastOrIRegister #-}
fastOrIRegister :: IOUArray IReg Word8 -> IReg -> Word8 -> IO ()
fastOrIRegister r i v = fastModifyIRegister r i (v .|.)
