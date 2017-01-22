{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Intel where

import Text.Parsec
import Control.Applicative
import Numeric (readHex)
import System.IO
import Control.Monad
import Data.Array.IO
import Control.Exception.Base

import Data.Word
import Data.Bits

hexWord8 :: Stream s m Char => ParsecT s u m Word8
hexWord8 = fromHex <$> count 2 hexDigit

hexWord16 :: Stream s m Char => ParsecT s u m Word16
hexWord16 = fromHex <$> count 4 hexDigit

fromHex :: (Num a, Eq a) => String -> a
fromHex = fst . head . readHex

data IntelHexRecord = IntelHexRecord Word8 Word16 Word8 [Word8] Word8 deriving Show

line :: Stream s m Char => ParsecT s u m IntelHexRecord
line = do
    char ':'
    bc   <- hexWord8
    addr <- hexWord16
    rt   <- hexWord8
    rd   <- count (fromIntegral bc) hexWord8
    cs   <- hexWord8
    return $ IntelHexRecord bc addr rt rd cs

readIntel :: IOUArray Int Word8 -> FilePath -> IO ()
readIntel arr fileName = do
    handle <- openFile fileName ReadMode
    text   <- hGetContents handle
    --ram    <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)
    forM_ (lines text) $ \l -> do
        let Right u = parse line "" l
        let IntelHexRecord len addr rec bytes check = u
        let s = len+fromIntegral addr
                   +(fromIntegral $ addr `shift` (-8))
                   +rec
                   +sum bytes
                   +check
        assert (s==0) $ return ()
        when (rec==0) $
                forM_ (zip [0..] bytes) $ \(i, b) -> do
                    let dst = fromIntegral (addr+fromIntegral i)
                    writeArray arr dst b

{-
main = do
    ram <- readIntel "6502test.hex"
    bytes <- getElems ram
    print bytes
 -}   
