{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Memory where

import Data.Word
import Data.Bits
import Data.Data
import Control.Lens

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

data MemoryType = TIA | RAM | RIOT | ROM deriving Show

{-# INLINE memoryType #-}
memoryType :: Word16 -> MemoryType
memoryType a | testBit a 12      = ROM
memoryType a | not (testBit a 7) = TIA
memoryType a | testBit a 9       = RIOT
memoryType _                     = RAM

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
-- http://blog.kevtris.org/blogfiles/Atari%202600%20Mappers.txt

data BankMode = UnBanked | ModeF6 | ModeF8 | Mode3F deriving (Show, Data, Typeable)
data BankState = NoBank | BankF6 !Word16 | BankF8 !Word16 | Bank3F !Word16 deriving Show

{-
data Memory = Memory {
    _bankMode :: !BankMode
--    _bankOffset :: !Word16
}

$(makeLenses ''Memory)
-}
