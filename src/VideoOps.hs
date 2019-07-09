{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Strict #-}

module VideoOps(bit) where

import Data.Word
import Data.Bits hiding (bit) -- XXX check this

{-# INLINE bit #-}
bit :: Int -> Bool -> Word8
bit _ False = 0
bit n True  = 1 `shift` n

