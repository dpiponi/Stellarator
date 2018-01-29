{-# LANGUAGE BinaryLiterals #-}
module BitManips where

import Data.Word
import Data.Bits
import Data.Array.Unboxed

slowReverse :: Word8 -> Word8
slowReverse n = ((n .&. 0b1) `shift` 7) .|.
                (( n .&. 0b10) `shift` 5) .|.
                (( n .&. 0b100) `shift` 3) .|.
                (( n .&. 0b1000) `shift` 1) .|.
                (( n .&. 0b10000) `shift` (-1)) .|.
                (( n .&. 0b100000) `shift` (-3)) .|.
                (( n .&. 0b1000000) `shift` (-5)) .|.
                (( n .&. 0b10000000) `shift` (-7))

reverseTable :: UArray Word8 Word8
reverseTable = listArray (0, 0xff) $ map slowReverse [0..0xff :: Word8]

assemblePlayFieldFwd :: Word8 -> Word8 -> Word8 -> Word64
assemblePlayFieldFwd pf0 pf1 pf2 = (fromIntegral pf0 `shift` (-4)) .|.
                                   ((fromIntegral (reverseTable!pf1)) `shift` 4) .|.
                                   (fromIntegral pf2 `shift` 12) .|.
                                   (fromIntegral (pf0 .&. 0xf0) `shift` 16) .|.
                                   ((fromIntegral (reverseTable!pf1)) `shift` 24) .|.
                                   (fromIntegral pf2 `shift` 32)

assemblePlayFieldRev :: Word8 -> Word8 -> Word8 -> Word64
assemblePlayFieldRev pf0 pf1 pf2 = (fromIntegral pf0 `shift` (-4)) .|.
                                   ((fromIntegral (reverseTable!pf1)) `shift` 4) .|.
                                   (fromIntegral pf2 `shift` 12) .|.
                                   ((fromIntegral (reverseTable!pf0)) `shift` 36) .|.
                                   (fromIntegral pf1 `shift` 28) .|.
                                   ((fromIntegral (reverseTable!pf2)) `shift` 20)

assemblePlayfield :: Bool -> Word8 -> Word8 -> Word8 -> Word64
assemblePlayfield False = assemblePlayFieldFwd
assemblePlayfield True = assemblePlayFieldRev

{-
test = do
    forM_ [4..7] $ \i ->
        putStrLn $ showIntAtBase 2 intToDigit (assemblePlayFieldRev (1 `shift` i) 0 0) ""
    forM_ [7, 6..0] $ \i ->
        putStrLn $ showIntAtBase 2 intToDigit (assemblePlayFieldRev 0 (1 `shift` i) 0) ""
    forM_ [0..7] $ \i ->
        putStrLn $ showIntAtBase 2 intToDigit (assemblePlayFieldRev 0 0 (1 `shift` i)) ""
    forM_ [4..7] $ \i ->
        putStrLn $ showIntAtBase 2 intToDigit (assemblePlayFieldRev (1 `shift` i) 0 0) ""
    forM_ [7, 6..0] $ \i ->
        putStrLn $ showIntAtBase 2 intToDigit (assemblePlayFieldRev 0 (1 `shift` i) 0) ""
    forM_ [0..7] $ \i ->
        putStrLn $ showIntAtBase 2 intToDigit (assemblePlayFieldRev 0 0 (1 `shift` i)) ""
-}
