module CPU where

import AcornAtom
import Data.Bits hiding (bit)
import Data.Word
import Prelude hiding (last, and)
import Control.Monad

-- {-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

-- {-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

-- {-# INLINE iz #-}
iz :: Integral a => a -> Int
iz = fromIntegral

-- {-# INLINABLE setN #-}
setN :: Word8 -> MonadAcorn ()
setN r = putN $ r >= 0x80

-- {-# INLINABLE setZ #-}
setZ :: Word8 -> MonadAcorn ()
setZ r = putZ $ r == 0

-- {-# INLINABLE setNZ #-}
setNZ :: Word8 -> MonadAcorn Word8
setNZ r = setN r >> setZ r >> return r

-- {-# INLINABLE setNZ_ #-}
setNZ_ :: Word8 -> MonadAcorn ()
setNZ_ r = setN r >> setZ r

discard :: MonadAcorn Word8 -> MonadAcorn ()
discard = void

-- {-# INLINE make16 #-}
make16 :: Word8 -> Word8 -> Word16
make16 lo0 hi0 = (i16 hi0 `shift` 8)+i16 lo0

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

-- {-# INLINABLE lo #-}
lo :: Word16 -> Word8
lo = i8

-- {-# INLINABLE hi #-}
hi :: Word16 -> Word8
hi a = i8 (a `shift` (-8))
