{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.AhoCorasick
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import System.IO
import Data.Word
import Control.Monad
import Data.List
import System.Environment
import Data.Bits
import Control.Lens

--testString = map BS.c2w $ take 16384 (cycle "ushers")
--example1 = mapM_ print $ findAll simpleSM testString where
--    simpleSM = makeSimpleStateMachine $ map (map BS.c2w) ["he","she","his","hers"]

data Evidence = Evidence {
    _forE0 :: Float,
    _for3F :: Float,
    _forF6 :: Float,
    _forF8 :: Float,
    _forNo :: Float
} deriving Show

$(makeLenses ''Evidence)

i16 :: Word8 -> Word16
i16 = fromIntegral

asWord16 :: [Word8] -> [Word16]
asWord16 []                 = []
asWord16 [_]                = []
asWord16 (b0 : bs@(b1 : _)) = i16 b0+(i16 b1 `shift` 8) : asWord16 bs

runId :: [Word8] -> Evidence -> IO Evidence
runId bytes evidence = runId' (asWord16 bytes) evidence

runId' :: [Word16] -> Evidence -> IO Evidence
runId' [] evidence = return evidence
runId' (w : ws) evidence = do
    evidence' <- wordId (w .&. 0xfff) evidence
    runId' ws evidence'

wordId :: Word16 -> Evidence -> IO Evidence
wordId addr   evidence | addr >= 0xfe0 && addr < 0xff8 = return (evidence & forE0 +~ 2)
wordId 0x3f85 evidence = return (evidence & for3F +~ 20)
wordId addr   evidence | addr >= 0xff8 && addr < 0xffa = return (evidence & forF8 +~ 15)
wordId addr   evidence | addr >= 0xff6 && addr < 0xffa = return (evidence & forF6 +~ 1)
wordId _      evidence = return evidence

fromSize :: Int -> Evidence -> Evidence
fromSize 2048 evidence = evidence & forNo +~ 100 & forE0 -~ 100 & for3F -~ 100 & forF6 -~ 100 & forF8 -~ 100
fromSize 4096 evidence = evidence & forNo +~ 100 & forE0 -~ 100 & for3F -~ 100 & forF6 -~ 100 & forF8 -~ 100
fromSize 8192 evidence = evidence & forF8 +~ 1 & forE0 +~ 1 & for3F +~ 5 & forF6 -~ 100 & forNo -~ 100
fromSize 16384 evidence = evidence & forF6 +~ 100 & forE0 -~ 100 & for3F -~ 100 & forF8 -~ 100 & forNo -~ 100
fromSize _ _ = error "Weird ROM size"

data BankType = ForNo | ForF6 | ForF8 | ForE0 | For3F deriving (Read, Show)

classify :: Evidence -> BankType
classify (Evidence fore0 for3f forf6 forf8 forno) =
    [ForE0, For3F, ForF6, ForF8, ForNo] !! (head (sortOn (\i -> -[fore0, for3f, forf6, forf8, forno]!!i) [0..4]))

main :: IO ()
main = do
    --handle <- openBinaryFile "roms/miner.bin" ReadMode
    handle1 <- openBinaryFile "BANK_TYPES" ReadMode
    bankDataString <- hGetContents handle1
    let bankData = read bankDataString :: [(String, BankType)]
    forM_ bankData $ \(fileName, bankType) -> do
        --print fileName
        --args <- getArgs
        --let [fileName] = args
        handle <- openBinaryFile ("roms/"++fileName) ReadMode
        --handle <- openBinaryFile "roms/midnightmagic.bin" ReadMode
        contents <- hGetContents handle
        putStrLn $ "file =" ++ fileName ++ " len = " ++ show (length contents)
        let contents' = map BS.c2w contents
        let evidence = fromSize (length contents) Evidence { _forE0 = 0, _for3F = 0, _forF6 = 0, _forF8 = 0, _forNo = 0 }
        evidence' <- runId contents' evidence
        print (fileName, evidence', bankType, classify evidence')
    {-
    let simpleSM = makeSimpleStateMachine $ [
                    [0xe0, 0xff],   -- 0xffe0 - E0
                    [0xe1, 0xff],   -- 0xffe0 - E0
                    [0xe2, 0xff],   -- 0xffe0 - E0
                    [0xe3, 0xff],   -- 0xffe0 - E0
                    [0xe4, 0xff],   -- 0xffe0 - E0
                    [0xe5, 0xff],   -- 0xffe0 - E0
                    [0xe6, 0xff],   -- 0xffe0 - E0
                    [0xe7, 0xff],   -- 0xffe0 - E0
                    [0xe8, 0xff],   -- 0xffe0 - E0
                    [0xe9, 0xff],   -- 0xffe0 - E0
                    [0xea, 0xff],   -- 0xffe0 - E0
                    [0xeb, 0xff],   -- 0xffe0 - E0
                    [0xec, 0xff],   -- 0xffe0 - E0
                    [0xed, 0xff],   -- 0xffe0 - E0
                    [0xee, 0xff],   -- 0xffe0 - E0
                    [0xf0, 0xff],   -- 0xffe0 - E0
                    [0xf1, 0xff],   -- 0xffe0 - E0
                    [0xf2, 0xff],   -- 0xffe0 - E0
                    [0xf3, 0xff],   -- 0xffe0 - E0
                    [0xf4, 0xff],   -- 0xffe0 - E0
                    [0xf5, 0xff],   -- 0xffe0 - E0
                    [0xf6, 0xff],   -- 0xffe0 - E0
                    [0xf7, 0xff],   -- 0xffe0 - E0
                    [0xa9, 0x00, 0x85, 0x3f],   -- lda #0, sta 3f - 3F
                    [0xa9, 0x01, 0x85, 0x3f],   -- lda #1, sta 3f - 3F
                    [0xa9, 0x02, 0x85, 0x3f]    -- lda #2, sta 3f - 3F
                ]
    mapM_ print $ findAll simpleSM contents'
    -}
