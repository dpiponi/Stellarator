{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Memory(MemoryType(..),
              isTIA,
              isRIOT,
              isROM,
              isRAM,
              BankState(..),
              bankAddress,
              memoryType,
              bankSwitch,
              BankMode(..)
             ) where

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

i16 :: Integral a => a -> Word16
i16 = fromIntegral

iz :: Integral a => a -> Int
iz = fromIntegral

{-# INLINE bankSwitch #-}
-- | bankSwitch sees the full 6507 address
-- i.e. it is in range 0x0000-0x1fff
bankSwitch :: Word16 -> Word8 -> BankState -> BankState
bankSwitch _      _    NoBank           = NoBank
bankSwitch 0x1ff8 _    (BankF8 _)       = BankF8 0x0000
bankSwitch 0x1ff9 _    (BankF8 _)       = BankF8 0x1000
bankSwitch 0x1ff6 _    (BankF6 _)       = BankF6 0x0000
bankSwitch 0x1ff7 _    (BankF6 _)       = BankF6 0x1000
bankSwitch 0x1ff8 _    (BankF6 _)       = BankF6 0x2000
bankSwitch 0x1ff9 _    (BankF6 _)       = BankF6 0x3000

-- My implementation of 3F doesn't fit the description at
-- http://blog.kevtris.org/blogfiles/Atari%202600%20Mappers.txt
-- I switch bank on a read or write at 0x3f.
-- I don't switch bank on other values.
bankSwitch 0x3f   bank (Bank3F _)       = Bank3F (i16 bank `shift` 11)
bankSwitch addr   bank state@(Bank3F _) = state

bankSwitch _      _    state            = state

{-# INLINE bankAddress #-}
-- | bankAddress sees the full 6507 address
-- i.e. it is in range 0x0000-0x1fff
-- though we only expect to see values in range 0x1000-0x1fff
-- as we only reach this function if the 6507 is reading from ROM.
bankAddress :: BankState -> Word16 -> Int
bankAddress NoBank          addr = iz (addr .&. 0xfff)
bankAddress (BankF8 offset) addr = ((iz addr .&. 0xfff)+iz offset)
bankAddress (BankF6 offset) addr = ((iz addr .&. 0xfff)+iz offset)
bankAddress (Bank3F _)      addr | addr > 0x1800 = iz addr
bankAddress (Bank3F offset) addr = ((iz addr .&. 0x7ff)+iz offset)
