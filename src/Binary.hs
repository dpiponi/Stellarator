{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Binary where

import System.IO
import Control.Monad
import Data.Array.IO
import Data.Char
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

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

readFont :: FilePath -> IO (Ptr Word8)
readFont filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let d = map (fromIntegral . ord) contents
    let e = filter (\x -> x == 88 || x == 32) d ::[Int]
    let f = map (\x -> if x == 88 then 0xff else 0x00) e
--     return f
    fontData <- mallocBytes (256*96+10000) :: IO (Ptr Word8)
    forM_ [0..256*96-1] $ \i -> do
        pokeElemOff fontData i (f !! i)
    return fontData
--     print f
--     print $ length f

--     forM_ [0..7] $ \i -> do
--         forM_ [0..31] $ \j -> do
--             putStrLn "["
--             forM_ [0..11]$ \l -> do
--                 putStr "\""
--                 forM_ [0..7]$ \m -> do
--                     let addr = (i*12+l)*256+(j*8+m)
--                     putStr $ (if f!!addr > 0 then "*" else " ")
--                 putStrLn "\","
--             putStrLn "],"
--     return f
