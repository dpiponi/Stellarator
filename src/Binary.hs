{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Binary where

import System.IO
import Control.Monad
import Data.Array.IO
import Numeric

import Data.Word
import qualified Data.ByteString.Internal as BS (c2w)

readBinary :: IOUArray Int Word8 -> FilePath -> Word16 -> IO ()
readBinary arr filename origin = do
    handle <- openBinaryFile filename ReadMode
    contents <- hGetContents handle

    putStrLn $ "ROM size = " ++ showHex (length contents) ""

    forM_ (zip [0..] contents) $ \(i, c) ->
        writeArray arr (i+fromIntegral origin) (BS.c2w c)
