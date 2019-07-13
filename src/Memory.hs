{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Memory(MemoryType(..),
              memoryType
             ) where

import Data.Word

data MemoryType = RAM | ROM | PPIA | VIA deriving Show

{-# INLINE memoryType #-}
memoryType :: Word16 -> MemoryType
memoryType a | a >= 0xb000 && a < 0xb004 = PPIA
memoryType a | a >= 0xb800 && a < 0xbc00 = VIA
memoryType a | a < 0xa000                = RAM
memoryType _                             = ROM
