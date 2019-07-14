{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Emulation where

import Asm hiding (a, s, x)
import AcornAtom
import Control.Lens hiding (set, op, index, noneOf)
import Control.Monad.Reader
import Data.Maybe
import Data.Char
import System.IO
import Data.Array.IO hiding (index)
import Data.Bits hiding (bit)
import Data.ByteString hiding (putStrLn, putStr, index)
import Foreign.Ptr
import Text.Printf
import Foreign.Storable
import Text.Parsec
import Data.IORef
import Data.Int
import CPU
import Data.Word
import DebugState
import Control.Concurrent
import Disasm hiding (make16)
import Display
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Foreign.Ptr
import Stella
import Memory
import Metrics
import Numeric
import Prelude hiding (last, and)
import System.Clock
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW hiding (getTime)
#if TRACE
import Data.Array.Storable
#endif

readMemory :: Word16 -> MonadAcorn Word8
writeMemory :: Word16 -> Word8 -> MonadAcorn ()
illegal :: Word8 -> MonadAcorn ()

--
-- {-# INLINE readMemory #-}
readMemory addr' = do
    let addr = addr'
    byte <- pureReadMemory (memoryType addr) addr

    return byte

-- {-# INLINE writeMemory #-}
writeMemory addr' v = do
    let addr = addr'
    pureWriteMemory (memoryType addr) addr v

-- {-# INLINE tick #-}
tick :: Int -> MonadAcorn ()
tick n = do
    modifyClock id (+ fromIntegral n)
    c <- useClock id -- XXXXXXXXXXXXXXXXXXXXXX
    when (c `mod` 16667 == 0) $ do
        renderDisplay
--         liftIO $ print $ fromIntegral c/1000000

-- {-# INLINE debugStr #-}
debugStr _ _ = return ()
-- {-# INLINE debugStrLn #-}
debugStrLn _ _ = return ()

-- Host instruction stuff..
writeWord32 :: Word16 -> Word32 -> MonadAcorn ()
writeWord32 i w = do
    writeMemory i (fromIntegral w)
    writeMemory (i+1) (fromIntegral $ w `shift` (-8))
    writeMemory (i+2) (fromIntegral $ w `shift` (-16))
    writeMemory (i+3) (fromIntegral $ w `shift` (-24))

hostFileName :: String -> MonadAcorn String
hostFileName name = return name
-- hostFileName name@(_ : '.' : _) = return name
--     dir <- use currentDirectory
--     return $ dir : '.' : name

saveBlock :: Word16 -> String -> MonadAcorn ()
saveBlock blockAddr hostName = do
    startData32 <- word32At (blockAddr+0xa)
    endData32 <- word32At (blockAddr+0xe)
    let start = i16 startData32
    let end = i16 endData32
    liftIO $ putStr $ printf " Save %04x:%04x to '%s'" start end hostName
    h <- liftIO $ openBinaryFile hostName WriteMode
    forM_ [start..end-1] $ \i -> do
        x <- readMemory i
        liftIO $ hPutChar h (BS.w2c x)
    liftIO $ hClose h

loadFile :: Word16 -> String -> MonadAcorn ()
loadFile blockAddr hostName = do
    loadAddr32 <- word32At (blockAddr+0x2)
    execAddr32 <- word32At (blockAddr+0x6)
    startData32 <- word32At (blockAddr+0xa)
    addressType <- readMemory (blockAddr+0x6)
    -- If caller-specified execution address ends in zero
    -- use user-specified load address
    -- otherwise use load address in file
    let start = fromIntegral loadAddr32
    h <- liftIO $ openBinaryFile hostName ReadMode
    bytes <- liftIO $ B.hGetContents h
    let len = B.length bytes
    let end = start+fromIntegral len
    liftIO $ putStr $ printf " Load %04x:%04x from '%s'" start end hostName
    forM_ (Prelude.zip [start..end-1] (Prelude.map BS.w2c $ B.unpack bytes)) $ \(i, d) -> writeMemory i (BS.c2w d)
    liftIO $ print "Done"
    liftIO $ hClose h

data Command = LOAD String Int -- <-- XXX needs to me Maybe Int
             | SAVE String Int Bool Int Int Int
             | RUN String -- XXX pass args

decimal :: ParsecT String u Identity Int
decimal = do
    digits <- many1 digit
    return $ read digits

number :: Stream s m t => Int -> ParsecT s u m Char -> ParsecT s u m Int
number base baseDigit
    = do { digits <- many1 baseDigit
         ; let n = Prelude.foldl (\x d -> base*x + (digitToInt d)) 0 digits
         ; seq n (return n)
         }

filename :: ParsecT String u Identity String
filename = (char '"' >> (many1 (noneOf "\"") <* char '"'))
           <|> many1 (noneOf " ")

ignoreCase :: Stream s m Char => [Char] -> ParsecT s u m [Char]
ignoreCase [] = return []
ignoreCase (c : cs) | isUpper c = do
    m <- char (toLower c) <|> char c
    ms <- ignoreCase cs
    return (m : ms)
ignoreCase (c : cs) | isLower c = do
    m <- char '.' <|> char c <|> char (toUpper c)
    if m == '.'
        then
            return "."
        else do
            ms <- ignoreCase cs
            return (m : ms)
ignoreCase (c : cs) = do
    m <- char c
    ms <- ignoreCase cs
    return (m : ms)

removeStars :: String -> String
removeStars ('*' : cs) = removeStars cs
removeStars cs = cs

{-# INLINE i32 #-}
i32 :: Integral a => a -> Word32
i32 = fromIntegral

{-# INLINE make32 #-}
make32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
make32 b0 b1 b2 b3 = (i32 b3 `shift` 24)+(i32 b2 `shift` 32)+(i32 b1 `shift` 8)+i32 b0

{-# INLINABLE stringAt #-}
stringAt :: Word16 -> MonadAcorn String
stringAt addr = do
    let loop cmd i = do
                    byte <- readMemory (addr+i16 i)
                    if byte == 0x0d || byte == 0x00
                        then return cmd
                        else loop (cmd ++ [BS.w2c byte]) (i+1)
    loop "" 0

{-# INLINE word16At #-}
word16At :: Word16 -> MonadAcorn Word16
word16At addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{-# INLINE word32At #-}
word32At :: Word16 -> MonadAcorn Word32
word32At addr = do
    b0 <- readMemory addr
    b1 <- readMemory (addr+1)
    b2 <- readMemory (addr+2)
    b3 <- readMemory (addr+3)
    return $ make32 b0 b1 b2 b3

{-# INLINE putWord32 #-}
putWord32 :: Word16 -> Word32 -> MonadAcorn ()
putWord32 addr w = do
    writeMemory addr (i8 $ w)
    writeMemory (addr+1) (i8 (w `shift` (-8)))
    writeMemory (addr+2) (i8 (w `shift` (-16)))
    writeMemory (addr+3) (i8 (w `shift` (-24)))

{-# INLINE writeWord16 #-}
writeWord16 :: Word16 -> Word16 -> MonadAcorn ()
writeWord16 i w = do
    writeMemory i (fromIntegral w)
    writeMemory (i+1) (fromIntegral $ w `shift` (-8))

-- XXX Ignore case of commands
parseCommand :: ParsecT String u Identity Command
parseCommand = (LOAD <$> (ignoreCase "Load" >> spaces >> filename)
                     <*> option 0 (spaces >> number 16 hexDigit))
               <|>
               (SAVE <$> (ignoreCase "Save" >> spaces >> (filename <* spaces))
                     <*> (number 16 hexDigit <* spaces)
                     <*> option False (char '+' >> spaces >> return True)
                     <*> (number 16 hexDigit <* spaces)
                     <*> option 0 (number 16 hexDigit <* spaces)
                     <*> option 0 (number 16 hexDigit <* spaces))
               <|>
               (RUN <$> (ignoreCase "Run" >> spaces >> (filename <* spaces)))

{-# INLINABLE osfile #-}
osfile :: MonadAcorn ()
osfile = do
    a <- getA
    x <- getX
    y <- getY

    liftIO $ putStrLn $ printf "OSFILE A=%02x X=%02x Y=%02x" a x y

    let blockAddr = make16 x y
    stringAddr <- word16At blockAddr
    rawFilename <- stringAt stringAddr
    hostName <- hostFileName rawFilename
    
    -- Note that the 'end' field points to the last byte,
    -- not the first byte after the end.
    case a of
        0x00 -> saveBlock blockAddr hostName
        0xff -> loadFile blockAddr hostName

        _ -> error $ "Unknown OSFILE call " ++ show a ++ "," ++ show x ++ "," ++ show y


execStarCommand :: Command -> MonadAcorn ()
execStarCommand (LOAD filename loadAddress) = do
    h <- liftIO $ openBinaryFile filename ReadMode
    bytes <- liftIO $ B.hGetContents h
    let bytes' = B.unpack $ B.take 22 bytes
    let addr = i16 (bytes'!!16) + 256*i16 (bytes'!!17)
    let start = i16 (bytes'!!18) + 256*i16 (bytes'!!19)
    let len = i16 (bytes'!!20) + 256*i16 (bytes'!!21)
    liftIO $ putStrLn $ "Loading at " ++ showHex addr ""
    forM_ (Prelude.zip [addr..] (Prelude.drop 22 $ Prelude.map BS.w2c $ B.unpack bytes)) $ \(i, d) -> do
        writeMemory i (BS.c2w d)
    liftIO $ print "Done"
    liftIO $ hClose h
    p0 <- getPC
    putPC $ p0+2

execStarCommand (RUN filename) = do
    h <- liftIO $ openBinaryFile filename ReadMode
    bytes <- liftIO $ B.hGetContents h
    let bytes' = B.unpack $ B.take 22 bytes
    let addr = i16 (bytes'!!16) + 256*i16 (bytes'!!17)
    let start = i16 (bytes'!!18) + 256*i16 (bytes'!!19)
    let len = i16 (bytes'!!20) + 256*i16 (bytes'!!21)
    liftIO $ putStrLn $ "Loading at " ++ showHex addr ""
    liftIO $ putStrLn $ "Running from " ++ showHex start ""
    forM_ (Prelude.zip [addr..] (Prelude.drop 22 $ Prelude.map BS.w2c $ B.unpack bytes)) $ \(i, d) -> do
        writeMemory i (BS.c2w d)
    liftIO $ print "Done"
    liftIO $ hClose h
--     p0 <- getPC
    putPC start

execStarCommand (SAVE filename startAddress relative endAddress execAddress reloadAddress) = do
    --liftIO $ printf "*SAVE %s %08x %d %08x %08x" filename startAddress (if relative then 1 else 0::Int) endAddress execAddress
    putA 0
    -- Control block at &02EE
    putX 0xee
    putY 0x02
    let addrFilename = 0x200 :: Word16
    -- Write filename
    forM_ (Prelude.zip [addrFilename..] filename) $
            \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
    -- Terminate filename with zero
    writeMemory (fromIntegral addrFilename+fromIntegral (Prelude.length filename)) 0
    -- Write address of filename
    writeWord16 0x2ee addrFilename
    writeWord32 (0x2ee+2) (fromIntegral $ if reloadAddress == 0 then startAddress else reloadAddress)
    writeWord32 (0x2ee+6) (fromIntegral execAddress)
    writeWord32 (0x2ee+0xa) (fromIntegral startAddress)
    writeWord32 (0x2ee+0xe) (fromIntegral $ if relative then startAddress+endAddress else endAddress)
    osfile
    p0 <- getPC
    putPC $ p0+2
-- execStarCommand (RUN filename) = do
--     putA 0xff
--     -- Control block at &02EE
--     putX 0xee
--     putY 0x02
--     let addrFilename = 0x200 :: Word16
--     forM_ (Prelude.zip [addrFilename..] filename) $ \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
--     writeMemory (fromIntegral addrFilename+fromIntegral (Prelude.length filename)) 0
--     writeWord16 0x2ee addrFilename
--     -- Signal that we want to use load address alreday in file.
--     writeWord32 (0x2ee+6) 1
--     osfile
--     fileExec <- word32At (0x2ee+6)
--     liftIO $ putStrLn $ "Executing from 0x" ++ showHex fileExec ""
--     -- Fake JSR
--     p0 <- getPC
--     push $ hi (p0+1)
--     push $ lo (p0+1)
--     putPC (i16 fileExec)

{-# INLINABLE oscli #-}
oscli :: MonadAcorn ()
oscli = do
--     lo <- getX
--     hi <- getY
--     let addr = make16 lo hi
    let addr = 0x100
    cmd <- stringAt addr
    liftIO $ putStrLn $ printf "OSCLI: %s" cmd
    let cmd' = removeStars cmd
    let cmd'' = parse parseCommand "" cmd'
    case cmd'' of
        Right cmd''' -> execStarCommand cmd'''
        Left _ -> do
            liftIO $ putStrLn $  "Unknow * command:" ++ cmd
            p0 <- getPC
            putPC $ p0+2
    return ()

-- {- INLINE illegal -}
illegal i = do
    if i == 0x02
    then do
      pp <- getPC
      -- Retroactively fix PC
      putPC $ pp-1
      p0 <- getPC
      op <- readMemory (p0+1)
      if op == 0x04
        then oscli
        else do
          putPC $ p0+2
          liftIO $ putStrLn $ "Host call with op 0x" ++ showHex op ""
    else do
      dumpState
      error $ "Illegal opcode 0x" ++ showHex i ""

debugStr :: Int -> String -> MonadAcorn ()
debugStrLn :: Int -> String -> MonadAcorn ()

-- {-# INLINE incPC #-}
incPC :: MonadAcorn ()
incPC = addPC 1

-- {-# INLINABLE read16 #-}
read16 :: Word16 -> MonadAcorn Word16
read16 addr = do
    lo0 <- readMemory addr
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

-- {-# INLINABLE read16tick #-}
read16tick :: Word16 -> MonadAcorn Word16
read16tick addr = do
    tick 1
    lo0 <- readMemory addr
    tick 1
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

-- {-# INLINABLE read16zpTick #-}
read16zpTick :: Word8 -> MonadAcorn Word16
read16zpTick addr = do
    tick 1
    lo0 <- readMemory (i16 addr)
    tick 1
    hi0 <- readMemory (i16 addr+1)
    return $ make16 lo0 hi0

-- http://www.emulator101.com/6502-addressing-modes.html

-- Note, a 6502 performs a read or write *every* clock cycle
-- regardless of what instruction is being executed.

-- 6 clock cycles...
-- {-# INLINABLE writeIndX #-}
writeIndX :: Word8 -> MonadAcorn ()
writeIndX src = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    addrX <- read16zpTick (addr+index)

    tick 1
    writeMemory addrX src
    incPC

-- 3 clock cycles
-- {-# INLINABLE writeZeroPage #-}
writeZeroPage :: Word8 -> MonadAcorn ()
writeZeroPage src = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    writeMemory (i16 addr) src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeAbs #-}
writeAbs :: Word8 -> MonadAcorn()
writeAbs src = do
    addr <- getPC >>= read16tick

    tick 1
    writeMemory addr src
    addPC 2

-- 6 clock cycles
-- {-# INLINABLE writeIndY #-}
writeIndY :: Word8 -> MonadAcorn ()
writeIndY src = do
    tick 1
    index <- getY
    addr' <- getPC >>= readMemory

    addr <- read16zpTick addr'

    let (halfAddrY, addrY) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeZeroPageX #-}
writeZeroPageX :: Word8 -> MonadAcorn ()
writeZeroPageX src = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    writeMemory (i16 $ addr+index) src
    incPC

-- 4 clock cycles
-- {-# INLINABLE writeZeroPageY #-}
writeZeroPageY :: Word8 -> MonadAcorn ()
writeZeroPageY src = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    writeMemory (i16 $ addr+index) src
    incPC

-- 5 clock cycles
-- {-# INLINABLE writeAbsY #-}
writeAbsY :: Word8 -> MonadAcorn ()
writeAbsY src = do
    index <- getY
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrY, addrY) = halfSum addr index
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    addPC 2

-- 5 clock cycles
-- {-# INLINABLE writeAbsX #-}
writeAbsX :: Word8 -> MonadAcorn ()
writeAbsX src = do
    index <- getX
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrX, addrX) = halfSum addr index
    discard $ readMemory halfAddrX

    tick 1
    writeMemory addrX src
    addPC 2

-- 6 clock cycles
-- {-# INLINABLE readIndX #-}
readIndX :: MonadAcorn Word8
readIndX = do
    tick 1
    index <- getX
    addr0 <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr0)

    addr1 <- read16zpTick (addr0+index)

    tick 1
    incPC
    readMemory addr1

-- 3 clock cycles
-- {-# INLINABLE readZeroPage #-}
readZeroPage :: MonadAcorn Word8
readZeroPage = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)
    incPC
    return src

-- 2 clock cycles
-- {-# INLINABLE readImm #-}
readImm :: MonadAcorn Word8
readImm = do
    tick 1
    src <- getPC >>= readMemory
    incPC
    return src

-- XXX consider applicable ops like *>
-- 4 clock cycles
-- {-# INLINABLE readAbs #-}
readAbs :: MonadAcorn Word8
readAbs = do
    p0 <- getPC
    src <- (read16tick p0 <* tick 1) >>= readMemory
    addPC 2
    return src

-- 5-6 clock cycles
-- {-# INLINABLE readIndY #-}
readIndY :: MonadAcorn Word8
readIndY = do
    tick 1
    addr' <- getPC >>= readMemory

    addr <- read16zpTick addr'

    index <- getY
    let (halfAddrY, addrY) = halfSum addr index

    when (halfAddrY /= addrY) $ do
        tick 1
        discard $ readMemory halfAddrY

    tick 1
    src <- readMemory addrY
    incPC
    return src

-- 4 clock cycles
-- {-# INLINABLE readZeroPageX #-}
readZeroPageX :: MonadAcorn Word8
readZeroPageX = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    incPC
    readMemory (i16 $ addr+index)

-- 4 clock cycles
-- {-# INLINABLE readZeroPageY #-}
readZeroPageY :: MonadAcorn Word8
readZeroPageY = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    incPC
    readMemory (i16 $ addr+index)

-- 4-5 clock cycles
-- {-# INLINABLE readAbsX #-}
readAbsX :: MonadAcorn Word8
readAbsX = do
    index <- getX
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrX, addrX) = halfSum addr index
    when (halfAddrX /= addrX) $ do
            tick 1
            discard $ readMemory halfAddrX

    tick 1
    readMemory addrX

-- 4-5 clock cycles
-- {-# INLINABLE readAbsY #-}
readAbsY :: MonadAcorn Word8
readAbsY = do
    index <- getY
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrY, addrY) = halfSum addr index
    when ( halfAddrY /= addrY) $ do
            tick 1
            discard $ readMemory halfAddrY

    tick 1
    readMemory addrY

-- 2-4 clock cycles
-- {-# INLINABLE bra #-}
bra :: MonadAcorn Bool -> Bool -> MonadAcorn ()
bra getFlag value = do
    tick 1
    offset <- getPC >>= readMemory
    f <- getFlag
    incPC

    when (value == f) $ do
        tick 1
        discard $ getPC >>= readMemory

        oldP <- getPC
        let (halfAddr, addr) = halfSignedSum oldP offset
        when (halfAddr /= addr) $ do
                tick 1
                discard $ readMemory halfAddr
        putPC addr

-- 2 clock cycles
-- {-# INLINABLE set #-}
set :: (Bool -> MonadAcorn ()) -> Bool -> MonadAcorn ()
set putFlag value = do
    tick 1
    discard $ getPC >>= readMemory
    putFlag value

-- 2 clock cycles
-- {-# INLINABLE nop #-}
nop :: MonadAcorn ()
nop = do
    tick 1
    discard $ getPC >>= readMemory

{-
-- 3 clock cycles. Undocumented.
-- {-# INLINABLE nop #-}
dop :: MonadAcorn ()
nop = do
    tick 1
    discard $ getPC >>= readMemory
-}

-- 3 clock cycles
-- {-# INLINABLE jmp #-}
jmp :: MonadAcorn ()
jmp = getPC >>= read16tick >>= putPC

-- 5 clock cycles
-- NB address wraps around in page XXX
-- Not correct here.
-- Looks like the torture test might not catch this.
-- Aha! That's why ALIGN is used before addresses!
-- {-# INLINABLE jmp_indirect #-}
jmp_indirect :: MonadAcorn ()
jmp_indirect = do
    getPC >>= read16tick >>= read16tick >>= putPC

-- {-# INLINABLE uselessly #-}
uselessly :: m () -> m ()
uselessly = id

-- 5 clock cycles
-- {-# INLINABLE withZeroPage #-}
withZeroPage :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withZeroPage op = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)

    tick 1
    uselessly $ writeMemory (i16 addr) src

    tick 1
    op src >>= writeMemory (i16 addr)
    incPC

-- 2 clock cycles
-- {-# INLINABLE withAcc #-}
withAcc :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAcc op = do
    tick 1
    discard $ getPC >>= readMemory
    getA >>= op >>= putA

-- 6 clock cycles
-- {-# INLINE withAbs #-}
withAbs :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAbs op = do
    addr <- getPC >>= read16tick
    
    tick 1
    src <- readMemory addr

    tick 1
    uselessly $ writeMemory addr src

    tick 1
    dst <- op src
    addPC 2
    writeMemory addr dst

-- 6 clock cycles
withZeroPageX :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withZeroPageX op = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory
    let addrX = addr+index

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    src <- readMemory (i16 addrX)

    tick 1
    writeMemory (i16 addrX) src

    tick 1
    dst <- op src
    writeMemory (i16 addrX) dst
    incPC
 
-- 7 clock cycles
-- {-# INLINE withAbsX #-}
withAbsX :: (Word8 -> MonadAcorn Word8) -> MonadAcorn ()
withAbsX op = do
    p0 <- getPC
    index <- getX
    addr <- read16tick p0

    let (halfAddrX, addrX) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrX

    tick 1
    src <- readMemory addrX

    tick 1
    uselessly $ writeMemory addrX src

    tick 1
    addPC 2
    dst <- op src
    writeMemory addrX dst

-- 7 clock cycles
-- {-# INLINABLE brk #-}
brk :: MonadAcorn ()
brk = do
    tick 1
    p0 <- getPC
    incPC
    discard $ readMemory p0

    p1 <- getPC
    incPC
    tick 1
    push $ hi p1

    incPC
    tick 1
    push $ lo p1

    putB True
    incPC
    tick 1
    getP >>= push . (.|. 0x20) -- always on bit
    putI True

    read16tick 0xfffe >>= putPC -- irq/brk XXX

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
-- XXX not supported correctly for now
-- {-# INLINABLE irq #-}
irq :: MonadAcorn ()
irq = do
    fi <- getI
    if not fi
        then nmi False
        else return ()

-- {-# INLINABLE push #-}
push :: Word8 -> MonadAcorn ()
push v = do
    sp <- getS
    writeMemory (0x100+i16 sp) v
    putS (sp-1)

-- {-# INLINABLE pull #-}
pull :: MonadAcorn Word8
pull = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemory (0x100+i16 sp')

-- 3 clock cycles
-- {-# INLINABLE pha #-}
pha :: MonadAcorn ()
pha = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getA >>= push

-- 3 clock cycles
-- {-# INLINABLE php #-}
php :: MonadAcorn ()
php = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getP >>= push . (.|. 0x30)

-- 4 clock cycles
-- {-# INLINABLE plp #-}
plp :: MonadAcorn ()
plp = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= putP

-- 4 clock cycles
-- {-# INLINABLE pla #-}
pla :: MonadAcorn ()
pla = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= setNZ >>= putA

-- {-# INLINABLE nmi #-}
nmi :: Bool -> MonadAcorn ()
nmi sw = do
    p0 <- getPC
    push $ hi p0
    push $ lo p0
    putB sw
    getP >>= push . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk XXX
    tick 7

-- 6 clock cycles
-- {-# INLINABLE rti #-}
rti :: MonadAcorn ()
rti = do
    tick 1
    p0 <- getPC
    void $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100 + fromIntegral s)

    tick 1
    pull >>= putP

    make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull) >>= putPC

-- 6 clock cycles
-- {-# INLINABLE jsr #-}
jsr :: MonadAcorn ()
jsr = do
    tick 1
    p0 <- getPC
    pcl <- readMemory p0
    incPC

    tick 1
    s <- getS
    discard $ readMemory (0x100 + fromIntegral s)

    p2 <- getPC

    tick 1
    push $ hi p2

    tick 1
    push $ lo p2

    tick 1
    pch <- readMemory p2

    putPC $ make16 pcl pch

-- 6 clock cycles
-- {-# INLINABLE rts #-}
rts :: MonadAcorn ()
rts = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    p0 <- make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull)
    
    tick 1
    discard $ readMemory p0
    putPC (p0+1)

initState :: Int -> Int -> Int -> Int ->
             IOUArray Int Word8 ->
#if TRACE
             StorableArray Int Word8 ->
#endif
             IOUArray Int Word8 ->
             Word16 ->
             Window -> 
             GL.Program ->
             GL.AttribLocation ->
             GL.TextureObject ->
             GL.TextureObject ->
             Ptr Word8 ->
             Ptr Word8 ->
             Controllers ->
             IO AcornAtom
initState xscale' yscale' width height ram'
#if TRACE
            record'
#endif
            rom' initialPC window prog attrib initTex initLastTex initTextureData initLastTextureData controllerType = do
          stellaDebug' <- newIORef DebugState.start
          t <- liftIO $ getTime Realtime
          let nt = addTime t (1000000000 `div` 60)
          nextFrameTime' <- newIORef nt
          clock' <- newIORef 0
          -- debug' <- newIORef 8
#if TRACE
          recordPtr' <- newIORef 0
#endif
          boolArray' <- newArray (0, maxBool) False
          intArray' <- newArray (0, maxInt) 0      -- Overkill
          word64Array' <- newArray (0, maxWord64) 0
          word16Array' <- newArray (0, maxWord16) 0      -- Overkill
          word8Array' <- newArray (0, maxWord8) 0
          liftIO $ st word16Array' pc initialPC
          return $ AcornAtom {
              _nextFrameTime = nextFrameTime',
              _xscale = xscale',
              _yscale = yscale',
              _windowWidth = width,
              _windowHeight = height,
              _rom = rom',
#if TRACE
              _record = record',
              _recordPtr = recordPtr',
#endif
              _ram = ram',
              _stellaDebug = stellaDebug',
              _clock = clock',
              _boolArray = boolArray',
              _intArray = intArray',
              _word64Array = word64Array',
              _word16Array = word16Array',
              _word8Array = word8Array',
              _controllers = controllerType,
              _sdlWindow = window,
              _textureData = initTextureData,
              _lastTextureData = initLastTextureData,
              _tex = initTex,
              _lastTex = initLastTex,
              _glProg = prog,
              _glAttrib = attrib
          }

-- {-# INLINE pureReadRom #-}
pureReadRom :: Word16 -> MonadAcorn Word8
pureReadRom addr = do
    atari <- ask
    let m = atari ^. rom
--     liftIO $ putStrLn $ "rom read addr =" ++ showHex (iz addr) ""
    liftIO $ readArray m (iz addr - 0xa000) -- Rom starts ac 0xc000

-- {-# INLINE pureWriteRom #-}
-- | pureWriteRom sees address in full 6507 range 0x0000-0x1fff
-- You can write to Super Chip "ROM"
pureWriteRom :: Word16 -> Word8 -> MonadAcorn ()
pureWriteRom addr v = do
    atari <- ask
    let m = atari ^. rom
    liftIO $ writeArray m (iz addr - 0xa000) v

-- {-# INLINE pureReadMemory #-}
pureReadMemory :: MemoryType -> Word16 -> MonadAcorn Word8
pureReadMemory PPIA addr = do
    bits <- case addr of
        0xb000 -> load ppia0
        0xb001 -> do
            row <- load keyboard_row
            bits <- load (keyboard_matrix + fromIntegral row)
--             liftIO $ putStrLn $ "Reading 0x" ++ showHex bits "" ++ " from PPIA: 0x" ++ showHex addr ""
            return bits

--        Port C - #B002
--        Output bits:      Function:
--             0          Tape output
--             1          Enable 2.4 kHz to cassette output
--             2          Loudspeaker
--             3          Not used
-- 
--        Input bits:       Function:
--             4          2.4 kHz input
--             5          Cassette input
--             6          REPT key (low when pressed)
--             7          60 Hz sync signal (low during flyback)

        0xb002 -> do
            c <- useClock id
            -- flyback
            -- 20000/frame PAL   16667/US
            let s = c `mod` 16667 -- clock cycles per 60 Hz
--             liftIO $ putStrLn $ "c = " ++ show s ++ ", s = " ++ show s
--          -- The 0x40 is the REPT key
--          -- PAL vertical blanking 1600 us
--          -- NTSC vertical blanking 1333us
            let bits = if s < 1333 then 0x40 else 0xc0
--             liftIO $ putStrLn $ "Reading 0x" ++ showHex bits "" ++ " from PPIA: 0x" ++ showHex addr ""
            return bits
        _ -> return 0
    return bits
pureReadMemory VIA _ = return 0
pureReadMemory ROM  addr = pureReadRom addr
pureReadMemory RAM  addr = do
    atari <- ask
    let m = atari ^. ram
--     liftIO $ putStrLn $ "ram read addr =" ++ showHex (iz addr) ""
    liftIO $ readArray m (iz addr)

displayChars :: String
displayChars = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]↑← !\"*$%&'()*+,-./0123456789:;<=>?"

translateChar :: Int -> Char
translateChar c | c < 64 = displayChars !! c
translateChar c | c < 128 = '.'
translateChar c | c < 192 = displayChars !! (c - 128)
translateChar _ = '.'


-- {-# INLINE pureWriteMemory #-}
pureWriteMemory :: MemoryType -> Word16 -> Word8 -> MonadAcorn ()
pureWriteMemory PPIA addr v = do
    case addr of
        0xb000 -> do
            store ppia0 v
            store keyboard_row (v .&. 0xf)
        _ -> return ()
pureWriteMemory VIA _ _ = return ()
pureWriteMemory ROM  addr v = pureWriteRom addr v
pureWriteMemory RAM  addr v = do
    atari <- ask
    let m = atari ^. ram
    let realAddress = iz addr
    liftIO $ writeArray m realAddress v


-- {-# INLINABLE dumpMemory #-}
dumpMemory :: MonadAcorn ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    liftIO $ putStr $ "PC = " ++ showHex regPC ""
    liftIO $ putStr $ "(PC) = "
    liftIO $ putStr $ showHex b0 "" ++ " "
    liftIO $ putStr $ showHex b1 "" ++ " "
    liftIO $ putStrLn $ showHex b2 ""
    let (_, mne, _) = disasm regPC [b0, b1, b2]
    liftIO $ putStrLn $ mne

-- {-# INLINABLE dumpRegisters #-}
dumpRegisters :: MonadAcorn ()
dumpRegisters = do
    regPC <- getPC
    liftIO $ putStr $ " pc = " ++ showHex regPC ""
    regP <- getP
    liftIO $ do
        putStr $ " flags = " ++ showHex regP ""
        putStr $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
        putStr $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
        putStr $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
        putStr $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
        putStr $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
        putStr $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
        putStr $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    liftIO $ putStr $ ") A = " ++ showHex regA ""
    regX <- getX
    liftIO $ putStr $ " X = " ++ showHex regX ""
    regY <- getY
    liftIO $ putStrLn $ " Y = " ++ showHex regY ""
    regS <- getS
    liftIO $ putStrLn $ " N = " ++ showHex regS ""

-- {-# INLINABLE dumpState #-}
dumpState :: MonadAcorn ()
dumpState = do
    dumpMemory
    dumpRegisters


renderDisplay :: MonadAcorn ()
renderDisplay = do
    window <- view sdlWindow
    prog <- view glProg
    attrib <- view glAttrib
    tex' <- view tex
    lastTex' <- view lastTex
    ptr <- view textureData
    lastPtr <- view lastTextureData
    windowWidth' <- view windowWidth
    windowHeight' <- view windowHeight
--     liftIO $ print "renderDisplay"
    -- Copy 6K of video RAM
    forM_ [0..6143::Int] $ \i -> do
        char <- readMemory (0x8000 + i16 i)
        liftIO $ pokeElemOff ptr (fromIntegral $ i) char
--     liftIO $ print "renderDisplay 1"
    liftIO $ updateTexture tex' ptr
    liftIO $ updateTexture lastTex' ptr
--     (w, h) <- getFramebufferSize window
    (w, h) <- liftIO $ getWindowSize window
    mode <- load ppia0
    let macRetinaKludge = 1
    liftIO $ draw (mode .&. 0xf0) (macRetinaKludge*w) (macRetinaKludge*h) prog attrib
--     liftIO $ print "renderDisplay 3"

    waitUntilNextFrameDue
--     liftIO $ swapInterval 0
    liftIO $ swapBuffers window
--     liftIO $ print "renderDisplay done"
    return ()

-- Note this fixes the *frame* rate.
-- It has nothing to do with the simulated clock
waitUntilNextFrameDue :: MonadAcorn ()
waitUntilNextFrameDue = do
    nextFrameTimeRef <- view nextFrameTime
    nextFrameTime' <- liftIO $ readIORef nextFrameTimeRef
    t <- liftIO $ getTime Realtime
    let frameTimeAfter = addTime nextFrameTime' (1000000000 `div` fps)
    liftIO $ writeIORef nextFrameTimeRef frameTimeAfter
    let TimeSpec {sec=secondsToGo, nsec=nanosecondsToGo} = diffTimeSpec nextFrameTime' t
    let timeToGo = fromIntegral secondsToGo+fromIntegral nanosecondsToGo/1e9 :: Double
    when (nextFrameTime' `gtTime` t) $ do
        let microSecondsToGo = 1000000.0 * timeToGo
        liftIO $ threadDelay $ floor microSecondsToGo

initHardware :: MonadAcorn ()
initHardware = do
    -- Clear keyboard
    forM_ [0..9::Int] $ \i -> do
        store (keyboard_matrix + fromIntegral i) 0xff
    pclo <- readMemory 0xfffc
    pchi <- readMemory 0xfffd
    let initialPC = fromIntegral pclo+(fromIntegral pchi `shift` 8)
    liftIO $ putStrLn $ "Starting at address: 0x" ++ showHex initialPC ""
    store pc initialPC
