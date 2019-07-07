{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Asm where

import Data.Word
import Data.Array.IO
import Data.Array.Base

newtype TypedIndex t = TO { unTyped :: Int } deriving (Ord, Ix, Eq, Num)

intim, p, a, x, y, s, oldGrp0, newGrp0, oldGrp1, newGrp1, timint, ppia0, keyboard_matrix, keyboard_row, keyboard_matrix_end :: TypedIndex Word8
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
timint = 10
ppia0 = 14
keyboard_row = 15
keyboard_matrix = 16
keyboard_matrix_end = 26

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

cxm0p, cxm1p, cxp0fb, cxp1fb, cxm0fb, cxm1fb, cxblpf, cxppmm, inpt0, inpt1, inpt2, inpt3, inpt4, inpt5 :: TypedIndex Word8
cxm0p = 0x200+0x00
cxm1p = 0x200+0x01
cxp0fb = 0x200+0x02
cxp1fb = 0x200+0x03
cxm0fb = 0x200+0x04
cxm1fb = 0x200+0x05
cxblpf = 0x200+0x06
cxppmm = 0x200+0x07
inpt0 = 0x200+0x08
inpt1 = 0x200+0x09
inpt2 = 0x200+0x0a
inpt3 = 0x200+0x0b
inpt4 = 0x200+0x0c
inpt5 = 0x200+0x0d

swcha, swchb, swacnt :: TypedIndex Word8
swcha = 0x300+0x80
swacnt = 0x300+0x81
swchb = 0x300+0x82

maxWord8 :: TypedIndex Word8
maxWord8 = swchb

trigger1, trigger2, delayP0, delayP1, delayBall, oldBall, newBall, pendingHmove, debugColours :: TypedIndex Bool
trigger1 = 0
trigger2 = 1
delayP0 = 2
delayP1 = 3
delayBall = 4
oldBall = 5
newBall = 6
pendingHmove = 7
debugColours = 8

kbd :: Int -> Int -> TypedIndex Bool
kbd i j = fromIntegral $ 8+i*6+j

maxBool :: TypedIndex Bool
maxBool = kbd 3 5

pf :: TypedIndex Word64
pf = 0

maxWord64 :: TypedIndex Word64
maxWord64 = pf

hpos, vpos, subtimer, interval, ppos0, ppos1, mpos0, mpos1, bpos :: TypedIndex Int
debug, ahead, xbreak, ybreak :: TypedIndex Int
hpos = 0
vpos = 1
subtimer = 2
interval = 3
ppos0 = 4
ppos1 = 5
mpos0 = 6
mpos1 = 7
bpos = 8
debug = 9
ahead = 10
xbreak = 11
ybreak = 12

maxInt :: TypedIndex Int
maxInt = ybreak

pc, bankOffset, pcStep, pcResp0, pcResp1, pcResm0, pcResm1, pcResbl, pcColup0, pcColup1, pcColupf, pcColubk, pcPf0, pcPf1, pcPf2 :: TypedIndex Word16
pc = 0
bankOffset = 1
pcStep = 2
pcResp0 = 3
pcResp1 = 4
pcResm0 = 5
pcResm1 = 6
pcResbl = 7
pcColup0 = 8
pcColup1 = 9
pcColupf = 10
pcColubk = 11
pcPf0 = 12
pcPf1 = 13
pcPf2 = 14

maxWord16 :: TypedIndex Word16
maxWord16 = pcPf2

{-# INLINE ld #-}
ld :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> IO a
ld arr idx = unsafeRead arr (unTyped idx)

{-# INLINE st #-}
st :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> a -> IO ()
st arr idx = unsafeWrite arr (unTyped idx)

{-# INLINE md #-}
md :: MArray IOUArray a IO => IOUArray (TypedIndex a) a -> TypedIndex a -> (a -> a) -> IO ()
md arr idx f = do { value <- unsafeRead arr (unTyped idx); unsafeWrite arr (unTyped idx) (f value) }

type Segment a = IOUArray (TypedIndex a) a

class Monad m => Reg t m where
    load :: TypedIndex t -> m t
    store :: TypedIndex t -> t -> m ()
    modify :: TypedIndex t -> (t -> t) -> m ()
    modify r f = do { value <- load r; store r (f value) }

(@=) :: Reg t m => TypedIndex t -> t -> m ()
(@=) = store

(@->) :: Reg t m => TypedIndex t -> TypedIndex t -> m ()
(@->) src dst = load src >>= (dst @=)

infixr 2 @= 
infix 2 @->
