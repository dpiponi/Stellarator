{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Binary where

import System.IO
import Control.Monad
import Data.Array.IO
import Data.Char

import Data.Word
import qualified Data.ByteString.Internal as BS (c2w)

readBinary :: IOUArray Int Word8 -> FilePath -> Word16 -> IO ()
readBinary arr filename origin = do
    handle <- openBinaryFile filename ReadMode
    contents <- hGetContents handle
    let romSize = length contents

    putStrLn $ filename ++ ": ROM size = " ++ show romSize ++ " bytes."

    forM_ (zip [0..] contents) $ \(i, c) ->
        writeArray arr (i+fromIntegral origin) (BS.c2w c)

readFont :: FilePath -> IO [Word8]
readFont filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let d = map (fromIntegral . ord) contents
    let e = filter (\x -> x == 88 || x == 32) d
    let f = map (\x -> if x == 88 then 255 else 0) e
    print f
    print $ length f
    return f
