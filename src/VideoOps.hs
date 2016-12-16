{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module VideoOps(stretchPlayer) where

import Data.Word
import Data.Bits

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True x = x
flipIf False x = 7-x

{- INLINE stretchPlayer' -}
stretchPlayer' :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer' !reflect 0b000 !o !bitmap = o < 8 && testBit bitmap (flipIf reflect $ fromIntegral o)
stretchPlayer' !reflect 0b001 !o !bitmap = (o < 8 || o >= 16 && o < 24) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b010 !o !bitmap = (o < 8 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b011 !o !bitmap = (o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b100 !o !bitmap = (o < 8 || o >= 64) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b101 !o !bitmap = o < 16 && testBit bitmap (flipIf reflect $ fromIntegral ((o `shift` (-1)) .&. 7))
stretchPlayer' !reflect 0b110 !o !bitmap = (o < 8 || o >= 32 && o < 40 || o >= 64) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' !reflect 0b111 !o !bitmap = o < 32 && testBit bitmap (flipIf reflect $ (fromIntegral ((o `shift` (-2)) .&. 7)))

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer !reflect !sizeCopies !o !bitmap =
    if o < 0 || o >= 72
        then False
        else stretchPlayer' reflect sizeCopies o bitmap
