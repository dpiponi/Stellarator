{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- http://nesdev.com/6502_cpu.txt
-- http://www.zimmers.net/anonftp/pub/cbm/documents/chipdata/64doc

module Disasm where

import Data.Word
import Control.Monad.State
import Data.Bits
import Text.Printf
import Data.Int
import Numeric

inHex8 :: Word8 -> String
inHex8 x = "0x" ++ showHex x ""

inHex16 :: Word16 -> String
inHex16 x = "0x" ++ showHex x ""

address8 :: Word8 -> String
address8 x = inHex8 x

make16 :: Word8 -> Word8 -> Word16
make16 lo hi = fromIntegral lo+(fromIntegral hi `shift` 8)

indirectX :: String -> [Word8] -> (Int, String, [Word8])
indirectX mne (b : bs) = (2, mne ++ " (" ++ inHex8 b ++ ", X)", bs)
indirectX _   [] = error "Not handled"

zeroPage :: String -> [Word8] -> (Int, String, [Word8])
zeroPage mne (b : bs) = (2, mne ++ " " ++ address8 b, bs)
zeroPage _   [] = error "Not handled"

absolute :: String -> [Word8] -> (Int, String, [Word8])
absolute mne (blo : bhi : bs) = (3, mne ++ " " ++ inHex16 (make16 blo bhi), bs)
absolute _   []               = error "Not handled"
absolute _   (_ : _)          = error "Not handled"

indirect :: String -> [Word8] -> (Int, String, [Word8])
indirect mne (blo : bhi : bs) = (3, mne ++ " (" ++ inHex16 (make16 blo bhi) ++ ")", bs)
indirect _   []               = error "Not handled"
indirect _   (_ : _)          = error "Not handled"

indirectY :: String -> [Word8] -> (Int, String, [Word8])
indirectY mne (b : bs) = (2, mne ++ " (" ++ inHex8 b ++ "), Y", bs)
indirectY _   [] = error "Not handled"

zeroPageX :: String -> [Word8] -> (Int, String, [Word8])
zeroPageX mne (b : bs) = (2, mne ++ " " ++ inHex8 b ++ ", X", bs)
zeroPageX _   [] = error "Not handled"

zeroPageY :: String -> [Word8] -> (Int, String, [Word8])
zeroPageY mne (b : bs) = (2, mne ++ " " ++ inHex8 b ++ ", Y", bs)
zeroPageY _   [] = error "Not handled"

absoluteY :: String -> [Word8] -> (Int, String, [Word8])
absoluteY mne (blo : bhi : bs) = (3, mne ++ " " ++ inHex16 (make16 blo bhi) ++ ", Y", bs)
absoluteY _   [] = error "Not handled"
absoluteY _   (_ : _)          = error "Not handled"

absoluteX :: String -> [Word8] -> (Int, String, [Word8])
absoluteX mne (blo : bhi : bs) = (3, mne ++ " " ++ inHex16 (make16 blo bhi) ++ ", X", bs)
absoluteX _   [] = error "Not handled"
absoluteX _   (_ : _)          = error "Not handled"

immediate :: String -> [Word8] -> (Int, String, [Word8])
immediate mne (b : bs) = (2, mne ++ " #" ++ inHex8 b, bs)
immediate _   [] = error "Not handled"

accumulator :: String -> [Word8] -> (Int, String, [Word8])
accumulator mne bs = (1, mne ++ " a", bs)

branch :: Word16 -> String -> [Word8] -> (Int, String, [Word8])
branch pc mne (b : bs) = 
    let offset = fromIntegral (fromIntegral b :: Int8)
        addr = pc+2+offset
    in (2, mne ++ " " ++ inHex16 addr, bs)
branch _  _   _        = error "Not handled"

withData01 :: Word8 -> String -> [Word8] -> (Int, String, [Word8])
withData01 bbb = do
    case bbb of
        0b000 -> indirectX
        0b001 -> zeroPage
        0b010 -> immediate
        0b011 -> absolute
        0b100 -> indirectY
        0b101 -> zeroPageX
        0b110 -> absoluteY
        0b111 -> absoluteX
        _     -> error "Impossible"

withData02 :: Word8 -> Bool -> String -> [Word8] -> (Int, String, [Word8])
withData02 bbb useY mne bs = case bbb of
    0b000 -> immediate mne bs
    0b001 -> zeroPage mne bs
    0b010 -> accumulator mne bs
    0b011 -> absolute mne bs
    0b101 -> if useY
                then zeroPageY mne bs
                else zeroPageX mne bs
    0b111 -> if useY
            then absoluteY mne bs
            else absoluteX mne bs 

    _ -> error "Unknown addressing mode"

dis_illegal :: Word8 -> [Word8] -> (Int, String, [Word8])
dis_illegal _ bs = (0, "error", bs)

disasm :: Word16 -> [Word8] -> (Int, String, [Word8])
disasm _ [] = error "Shouldn't happen"
disasm pc (b : bs) =
    case b of
        0x00 -> (1, "brk", bs)
        0x08 -> (1, "php", bs)
        0x10 -> branch pc "BPL" bs
        0x18 -> (1, "clc", bs)
        0x20 -> absolute "jsr" bs
        0x28 -> (1, "plp", bs)
        0x30 -> branch pc "BMI" bs
        0x38 -> (1, "sec", bs)
        0x40 -> (1, "rti", bs)
        0x48 -> (1, "pha", bs)
        0x50 -> branch pc "BVC" bs
        0x58 -> (1, "cli", bs)
        0x60 -> (1, "rts", bs)
        0x68 -> (1, "pla", bs)
        0x70 -> branch pc "bvs" bs
        0x78 -> (1, "sei", bs)
        0x88 -> (1, "dey", bs)
        0x8a -> (1, "txa", bs)
        0x90 -> branch pc "bcc" bs
        0x98 -> (1, "tya", bs)
        0x9a -> (1, "txs", bs)
        0xa8 -> (1, "tay", bs)
        0xaa -> (1, "tax", bs)
        0xb0 -> branch pc "bcs" bs
        0xb8 -> (1, "clv", bs)
        0xba -> (1, "tsx", bs)
        0xc8 -> (1, "iny", bs)
        0xca -> (1, "dex", bs)
        0xd0 -> branch pc "bne" bs
        0xd8 -> (1, "cld", bs)
        0xe8 -> (1, "iny", bs)
        0xea -> (1, "nop", bs)
        0xf0 -> branch pc "beq" bs
        0xf8 -> (1, "sed", bs)

        _ -> do
            let cc = b .&. 0b11
            case cc of
                0b00 -> do
                    let aaa = (b `shift` (-5)) .&. 0b111
                    let bbb = (b `shift` (-2)) .&. 0b111
                    case aaa of
                        0b001 -> withData02 bbb False "bit" bs
                        0b010 -> absolute "jmp" bs
                        0b011 -> indirect "jmp" bs
                        0b100 -> withData02 bbb False "sty" bs
                        0b101 -> withData02 bbb False "ldy" bs
                        0b110 -> withData02 bbb False "cpy" bs
                        0b111 -> withData02 bbb False "cpx" bs

                        _ -> dis_illegal b bs

                0b01 -> do
                    let aaa = (b `shift` (-5)) .&. 0b111
                    let bbb = (b `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> withData01 bbb "ora" bs
                        0b001 -> withData01 bbb "and" bs
                        0b010 -> withData01 bbb "xor" bs
                        0b011 -> withData01 bbb "adc" bs
                        0b100 -> withData01 bbb "sta" bs
                        0b101 -> withData01 bbb "lda" bs
                        0b110 -> withData01 bbb "cmp" bs
                        0b111 -> withData01 bbb "sbc" bs

                        _ -> dis_illegal b bs
                0b10 -> do
                    let aaa = (b `shift` (-5)) .&. 0b111
                    let bbb = (b `shift` (-2)) .&. 0b111
                    case aaa of

                        0b000 -> withData02 bbb False "asl" bs
                        0b001 -> withData02 bbb False "rol" bs
                        0b010 -> withData02 bbb False "lsr" bs
                        0b011 -> withData02 bbb False "ror" bs
                        0b100 -> withData02 bbb True "stx" bs
                        0b101 -> withData02 bbb True "ldx"  bs
                        0b110 -> withData02 bbb False "dec" bs
                        0b111 -> withData02 bbb False "inc" bs
                        _     -> error "Impossible"

                _ -> dis_illegal b bs

dis :: Int -> Word16 -> [Word8] -> IO ()
dis 0 _ _ = return ()
dis m pc bs = do
    let (n, mne, bs') = disasm pc bs
    printf "%04x " pc
    mapM_ (printf "%02x ") (take n bs)
    replicateM_ (3*(3-n)) $ putChar ' '
    putStrLn mne
    dis (m-1) (pc+fromIntegral n) bs'
