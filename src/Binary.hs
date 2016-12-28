{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Binary where

import System.IO
import Control.Monad
import Data.Array.IO
-- import Numeric
import Memory

import Data.Word
import qualified Data.ByteString.Internal as BS (c2w)

readBinary :: IOUArray Int Word8 -> FilePath -> Word16 -> IO BankMode
readBinary arr filename origin = do
    handle <- openBinaryFile filename ReadMode
    contents <- hGetContents handle
    let romSize = length contents

    putStrLn $ "ROM size = " ++ show romSize ++ " bytes."

    if romSize == 0x800

        then do
            forM_ (zip [0..] contents) $ \(i, c) -> do
                -- 2K ROM so double it
                writeArray arr (i+fromIntegral origin) (BS.c2w c)
                writeArray arr (i+0x800+fromIntegral origin) (BS.c2w c)
            return UnBanked

        else do
            forM_ (zip [0..] contents) $ \(i, c) ->
                writeArray arr (i+fromIntegral origin) (BS.c2w c)
            case romSize of
                0x1000 -> return UnBanked
                0x2000 -> return ModeF8
                0x4000 -> return ModeF6
                _      -> error "Unrecognised ROM size"
