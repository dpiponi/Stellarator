{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Memory(MemoryType(..),
              isTIA,
--              isRIOT,
--              isROM,
--              isRAM,
              BankState(..),
              bankAddress,
              bankStyleByName,
              memoryType,
              bankSwitch,
              bankWritable,
              initialBankState,
              BankMode(..)
             ) where

import Data.Word
import Data.Bits
import Data.Data

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

{-
{-# INLINE isRAM #-}
isRAM :: Word16 -> Bool
isRAM a = testBit a 7 && not (testBit a 9) && not (testBit a 12)

{-# INLINE isRIOT #-}
isRIOT :: Word16 -> Bool
isRIOT a = testBit a 7 && testBit a 9 && not (testBit a 12)

{-# INLINE isROM #-}
isROM :: Word16 -> Bool
isROM a = testBit a 12
-}

-- http://www.classic-games.com/atari2600/bankswitch.html
-- http://blog.kevtris.org/blogfiles/Atari%202600%20Mappers.txt

data BankMode = UnBanked
              | ModeF4 -- 32K
              | ModeF4SC -- 32K
              | ModeF6 -- 16K
              | ModeF6SC -- 16K
              | ModeF8 -- 8K
              | ModeF8SC -- 8K
              | ModeE0 -- 8K
              | Mode3F deriving (Show, Data, Typeable)

data BankState = NoBank
               | BankF4 !Word16
               | BankF4SC !Word16
               | BankF6 !Word16
               | BankF6SC !Word16
               | BankF8 !Word16
               | BankF8SC !Word16
               | BankE0 !Word16 !Word16 !Word16
               | Bank3F !Word16 deriving Show

i16 :: Integral a => a -> Word16
i16 = fromIntegral

iz :: Integral a => a -> Int
iz = fromIntegral

{-# INLINE bankSwitch #-}
-- | bankSwitch sees the full 6507 address
-- i.e. it is in range 0x0000-0x1fff
bankSwitch :: Word16 -> Word8 -> BankState -> BankState
bankSwitch    _         _        NoBank           = NoBank

bankSwitch    0x1ff8    _        (BankF8 _)       = BankF8 0x0000
bankSwitch    0x1ff9    _        (BankF8 _)       = BankF8 0x1000
bankSwitch    0x1ff8    _        (BankF8SC _)     = BankF8SC 0x0000
bankSwitch    0x1ff9    _        (BankF8SC _)     = BankF8SC 0x1000
bankSwitch    0x1ff6    _        (BankF6 _)       = BankF6 0x0000
bankSwitch    0x1ff7    _        (BankF6 _)       = BankF6 0x1000
bankSwitch    0x1ff8    _        (BankF6 _)       = BankF6 0x2000
bankSwitch    0x1ff9    _        (BankF6 _)       = BankF6 0x3000
bankSwitch    0x1ff6    _        (BankF6SC _)     = BankF6SC 0x0000
bankSwitch    0x1ff7    _        (BankF6SC _)     = BankF6SC 0x1000
bankSwitch    0x1ff8    _        (BankF6SC _)     = BankF6SC 0x2000
bankSwitch    0x1ff9    _        (BankF6SC _)     = BankF6SC 0x3000

#if 0
-- https://hackaday.io/project/12961-atari-2600-bankswitch-cartridge
-- See "Particularities of the 32K cartridge"
-- Bits 1 and 3 swapped
bankSwitch    0x1ff4    _        (BankF4 _)       = BankF4 0x0000
bankSwitch    0x1ff5    _        (BankF4 _)       = BankF4 0x1000
bankSwitch    0x1ff6    _        (BankF4 _)       = BankF4 0x4000
bankSwitch    0x1ff7    _        (BankF4 _)       = BankF4 0x5000
bankSwitch    0x1ff8    _        (BankF4 _)       = BankF4 0x2000
bankSwitch    0x1ff9    _        (BankF4 _)       = BankF4 0x3000
bankSwitch    0x1ffA    _        (BankF4 _)       = BankF4 0x6000
bankSwitch    0x1ffB    _        (BankF4 _)       = BankF4 0x7000
bankSwitch    0x1ff4    _        (BankF4SC _)       = BankF4SC 0x0000
bankSwitch    0x1ff5    _        (BankF4SC _)       = BankF4SC 0x1000
bankSwitch    0x1ff6    _        (BankF4SC _)       = BankF4SC 0x4000
bankSwitch    0x1ff7    _        (BankF4SC _)       = BankF4SC 0x5000
bankSwitch    0x1ff8    _        (BankF4SC _)       = BankF4SC 0x2000
bankSwitch    0x1ff9    _        (BankF4SC _)       = BankF4SC 0x3000
bankSwitch    0x1ffA    _        (BankF4SC _)       = BankF4SC 0x6000
bankSwitch    0x1ffB    _        (BankF4SC _)       = BankF4SC 0x7000
#else
bankSwitch    0x1ff4    _        (BankF4 _)       = BankF4 0x0000
bankSwitch    0x1ff5    _        (BankF4 _)       = BankF4 0x1000
bankSwitch    0x1ff6    _        (BankF4 _)       = BankF4 0x2000
bankSwitch    0x1ff7    _        (BankF4 _)       = BankF4 0x3000
bankSwitch    0x1ff8    _        (BankF4 _)       = BankF4 0x4000
bankSwitch    0x1ff9    _        (BankF4 _)       = BankF4 0x5000
bankSwitch    0x1ffA    _        (BankF4 _)       = BankF4 0x6000
bankSwitch    0x1ffB    _        (BankF4 _)       = BankF4 0x7000
bankSwitch    0x1ff4    _        (BankF4SC _)       = BankF4SC 0x0000
bankSwitch    0x1ff5    _        (BankF4SC _)       = BankF4SC 0x1000
bankSwitch    0x1ff6    _        (BankF4SC _)       = BankF4SC 0x2000
bankSwitch    0x1ff7    _        (BankF4SC _)       = BankF4SC 0x3000
bankSwitch    0x1ff8    _        (BankF4SC _)       = BankF4SC 0x4000
bankSwitch    0x1ff9    _        (BankF4SC _)       = BankF4SC 0x5000
bankSwitch    0x1ffA    _        (BankF4SC _)       = BankF4SC 0x6000
bankSwitch    0x1ffB    _        (BankF4SC _)       = BankF4SC 0x7000

bankSwitch    0x1fe0    _        (BankE0 _ b c)     = BankE0 0x0000 b c
bankSwitch    0x1fe1    _        (BankE0 _ b c)     = BankE0 0x0400 b c
bankSwitch    0x1fe2    _        (BankE0 _ b c)     = BankE0 0x0800 b c
bankSwitch    0x1fe3    _        (BankE0 _ b c)     = BankE0 0x0c00 b c
bankSwitch    0x1fe4    _        (BankE0 _ b c)     = BankE0 0x1000 b c
bankSwitch    0x1fe5    _        (BankE0 _ b c)     = BankE0 0x1400 b c
bankSwitch    0x1fe6    _        (BankE0 _ b c)     = BankE0 0x1800 b c
bankSwitch    0x1fe7    _        (BankE0 _ b c)     = BankE0 0x1c00 b c
bankSwitch    0x1fe8    _        (BankE0 a _ c)     = BankE0 a 0x0000 c
bankSwitch    0x1fe9    _        (BankE0 a _ c)     = BankE0 a 0x0400 c
bankSwitch    0x1fea    _        (BankE0 a _ c)     = BankE0 a 0x0800 c
bankSwitch    0x1feb    _        (BankE0 a _ c)     = BankE0 a 0x0c00 c
bankSwitch    0x1fec    _        (BankE0 a _ c)     = BankE0 a 0x1000 c
bankSwitch    0x1fed    _        (BankE0 a _ c)     = BankE0 a 0x1400 c
bankSwitch    0x1fee    _        (BankE0 a _ c)     = BankE0 a 0x1800 c
bankSwitch    0x1fef    _        (BankE0 a _ c)     = BankE0 a 0x1c00 c
bankSwitch    0x1ff0    _        (BankE0 a b _)     = BankE0 a b 0x0000
bankSwitch    0x1ff1    _        (BankE0 a b _)     = BankE0 a b 0x0400
bankSwitch    0x1ff2    _        (BankE0 a b _)     = BankE0 a b 0x0800
bankSwitch    0x1ff3    _        (BankE0 a b _)     = BankE0 a b 0x0c00
bankSwitch    0x1ff4    _        (BankE0 a b _)     = BankE0 a b 0x1000
bankSwitch    0x1ff5    _        (BankE0 a b _)     = BankE0 a b 0x1400
bankSwitch    0x1ff6    _        (BankE0 a b _)     = BankE0 a b 0x1800
bankSwitch    0x1ff7    _        (BankE0 a b _)     = BankE0 a b 0x1c00
#endif

-- My implementation of 3F doesn't fit the description at
-- http://blog.kevtris.org/blogfiles/Atari%202600%20Mappers.txt
-- I switch bank on a read or write at 0x3f.
-- I don't switch bank on other values.
bankSwitch    0x3f      bank     (Bank3F _)       = Bank3F (i16 bank `shift` 11)
bankSwitch    _         _        state@(Bank3F _) = state

bankSwitch    _         _        state            = state

{-# INLINE bankAddress #-}
-- | bankAddress sees the full 6507 address
-- i.e. it is in range 0x0000-0x1fff
-- though we only expect to see values in range 0x1000-0x1fff
-- as we only reach this function if the 6507 is reading from ROM.
-- Note that when using Super Chip RAM the first 128 bytes are supposed to be
-- write-only and the next are a read-only mirror.
-- In theory a read operation on the write-only section could write something.
-- I've not implemented that.
bankAddress :: BankState ->      Word16 -> Int
bankAddress    NoBank            addr   = iz (addr .&. 0xfff)
bankAddress    (BankF8 offset)   addr   = ((iz addr .&. 0xfff)+iz offset)
bankAddress    (BankF8SC offset) addr   = let zaddr = iz addr .&. 0xfff
                                          in if zaddr < 0x100 then (zaddr .&. 0x7f) else zaddr+iz offset
bankAddress    (BankF6 offset)   addr   = ((iz addr .&. 0xfff)+iz offset)
bankAddress    (BankF6SC offset) addr   = let zaddr = iz addr .&. 0xfff
                                          in if zaddr < 0x100 then (zaddr .&. 0x7f) else zaddr+iz offset
bankAddress    (BankF4 offset)   addr   = ((iz addr .&. 0xfff)+iz offset)
bankAddress    (BankF4SC offset) addr   = let zaddr = iz addr .&. 0xfff
                                          in if zaddr < 0x100 then (zaddr .&. 0x7f) else zaddr+iz offset

bankAddress    (BankE0 a b c)    addr =   let zaddr = iz addr .&. 0x3ff -- 1K blocks
                                          in case addr .&. 0x0c00 of
                                            0x0000 -> iz a+zaddr
                                            0x0400 -> iz b+zaddr
                                            0x0800 -> iz c+zaddr
                                            0x0c00 -> 0x1c00+zaddr
                                            _      -> error "Provably impossible"
bankAddress    (Bank3F _)        addr   | addr > 0x1800 = iz addr
bankAddress    (Bank3F offset)   addr   = ((iz addr .&. 0x7ff)+iz offset)

{-# INLINE bankWritable #-}
-- | bankAddress sees the full 6507 address
-- i.e. it is in range 0x0000-0x1fff
-- though we only expect to see values in range 0x1000-0x1fff
-- as we only reach this function if the 6507 is reading from ROM.
bankWritable :: BankState -> Word16 -> Bool
bankWritable    (BankF6SC _) addr = (addr .&. 0xfff) < 0x100
bankWritable    _            _    = False

bankStyleByName :: BankMode -> String -> BankMode
bankStyleByName _         "f8"   = ModeF8
bankStyleByName _         "f8sc" = ModeF8SC
bankStyleByName _         "f6"   = ModeF6
bankStyleByName _         "f6sc" = ModeF6SC
bankStyleByName _         "f4"   = ModeF4
bankStyleByName _         "f4sc" = ModeF4SC
bankStyleByName _         "e0"   = ModeE0
bankStyleByName _         "3f"   = Mode3F
bankStyleByName bankStyle _      = bankStyle

initialBankState :: BankMode -> BankState
initialBankState UnBanked = NoBank
initialBankState ModeF8   = BankF8 0x0000
initialBankState ModeF8SC = BankF8SC 0x0000
initialBankState ModeF6   = BankF6 0x0000
initialBankState ModeF6SC = BankF6SC 0x0000
initialBankState ModeF4   = BankF4 0x0000
initialBankState ModeF4SC = BankF4SC 0x0000
initialBankState ModeE0   = BankE0 0x0000 0x0000 0x0000
initialBankState Mode3F   = Bank3F 0x0000

