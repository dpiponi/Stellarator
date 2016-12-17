{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Strict #-}

module Memory where

import Data.Word
import Data.Bits
import Data.Data
import Control.Lens
import Data.Array.IO
import Data.Array.Unboxed

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

-- http://www.classic-games.com/atari2600/bankswitch.html

data BankMode = UnBanked | F6 | F8 deriving (Show, Data, Typeable)

data Memory = Memory {
    _ram :: IOUArray Int Word8,
    _rom :: IOUArray Int Word8,
    _bankMode :: !BankMode,
    _bankOffset :: !Word16
}

$(makeLenses ''Memory)
