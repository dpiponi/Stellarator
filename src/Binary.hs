{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Binary where

import Text.Parsec
import Control.Applicative
import Numeric (readHex)
import System.IO
import Control.Monad
import Data.Array.IO
import Control.Exception.Base

import Data.Word
import Data.Bits
import qualified Data.ByteString.Internal as BS (c2w, w2c)

readBinary :: IOUArray Int Word8 -> FilePath -> Word16 -> IO ()
readBinary arr filename origin = do
    --arr <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)
    handle <- openBinaryFile filename ReadMode
    contents <- hGetContents handle
--    n <- hGetArray handle arr 0xffff
--    putStrLn $ "Read " ++ show n ++ " bytes"
--    hClose handle

    forM_ (zip [0..] contents) $ \(i, c) ->
        writeArray arr (i+fromIntegral origin) (BS.c2w c)
