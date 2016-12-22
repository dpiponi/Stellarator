{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}

module VideoOps(compositeAndCollide,
                clampMissiles,
                stellaTick,
                bit) where

import Data.Word
import Data.Bits hiding (bit) -- XXX check this
import Atari2600
--import Stella.Sprites
import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.Array.Unboxed
import Data.Array.IO
import Stella.TIARegisters
-- import Control.Lens
--import Stella.Graphics
import DebugState
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import SDL.Video.Renderer
import TIAColors
import Stella.SDLState
import Metrics

{-# INLINABLE updatePos #-}
updatePos :: (Int, Int) -> (Int, Int)
updatePos (hpos0, vpos0) =
    let hpos' = hpos0+1
    in if hpos' < picx+160
        then (hpos', vpos0)
        else let vpos' = vpos0+1
             in (0, vpos') -- if vpos' < picy+screenScanLines
                -- then (0, vpos')
                -- else (0, 0)
                --

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True x = x
flipIf False x = 7-x

{- INLINE stretchPlayer' -}
stretchPlayer' :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer' reflect 0b000 o bitmap = o < 8 && testBit bitmap (flipIf reflect $ fromIntegral o)
stretchPlayer' reflect 0b001 o bitmap = (o < 8 || o >= 16 && o < 24) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' reflect 0b010 o bitmap = (o < 8 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' reflect 0b011 o bitmap = (o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' reflect 0b100 o bitmap = (o < 8 || o >= 64) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' reflect 0b101 o bitmap = o < 16 && testBit bitmap (flipIf reflect $ fromIntegral ((o `shift` (-1)) .&. 7))
stretchPlayer' reflect 0b110 o bitmap = (o < 8 || o >= 32 && o < 40 || o >= 64) && testBit bitmap (flipIf reflect $ fromIntegral (o .&. 7))
stretchPlayer' reflect 0b111 o bitmap = o < 32 && testBit bitmap (flipIf reflect $ (fromIntegral ((o `shift` (-2)) .&. 7)))

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer reflect sizeCopies o bitmap =
    if o < 0 || o >= 72
        then False
        else stretchPlayer' reflect sizeCopies o bitmap

clampMissiles :: Word8 -> Word8 -> MonadAtari ()
clampMissiles resmp0' resmp1' = do
    when (testBit resmp0' 1) $ do { ppos0' <- load s_ppos0; store s_mpos0 ppos0' }
    when (testBit resmp1' 1) $ do { ppos1' <- load s_ppos1; store s_mpos1 ppos1' }

-- Atari2600 programmer's guide p.22
{- INLINE missile0 -}
-- XXX Note that this updates mpos0 so need to take into account XXX
missile :: Word8 -> Word8 -> Int -> Word8 -> Bool
missile _       _      o _       | o < 0                  = False
missile _       _      _ resmp0' | testBit resmp0' 1      = False
missile _       enam0' _ _       | not (testBit enam0' 1) = False
missile nusiz0' enam0' o resmp0'                          = o < missileSize nusiz0'

-- Atari2600 programmer's guide p.40
{- INLINE player0 -}
player0 :: Segment Word8 -> IOUArray OReg Word8 -> Bool -> Word8 -> Int -> IO Bool
player0 _ _ _ _ o | o < 0 = return False
player0 word8r r delayP0' nusiz0' o = do
    let sizeCopies = 0b111 .&. nusiz0'
    grp0' <- if delayP0' then ld word8r oldGrp0 else ld word8r newGrp0
    refp0' <- ld word8r refp0
    return $ stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: Segment Word8 -> IOUArray OReg Word8 -> Bool -> Word8 -> Int -> IO Bool
player1 _ _ _ _ o | o < 0 = return False
player1 word8r r delayP1' nusiz1' o = do
    let sizeCopies = 0b111 .&. nusiz1'
    grp1' <- if delayP1' then ld word8r oldGrp1 else ld word8r newGrp1
    refp1' <- ld word8r refp1
    return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

{- INLINE ball -}
ball :: Bool -> Bool -> Bool -> Word8 -> Int -> Bool
ball _ _ _ _ o | o < 0 = False
ball delayBall' oldBall' newBall' ctrlpf' o = do
    let enabl' = if delayBall' then oldBall' else newBall'
    if enabl'
        then do
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
            o >= 0 && o < ballSize
        else False

{- INLINE playfield -}
playfield :: Segment Word8 -> Word8 -> Int -> IO Bool
playfield word8r ctrlpf' i | i >= 0  && i < 4  = flip testBit (i+4)  <$> ld word8r pf0
                      | i >=4   && i < 12 = flip testBit (11-i) <$> ld word8r pf1
                      | i >= 12 && i < 20 = flip testBit (i-12) <$> ld word8r pf2
playfield r ctrlpf' i | i >= 20 && i < 40 = playfield r ctrlpf' $ if testBit ctrlpf' 0 then 39-i else i-20
playfield _ _ _ = return $ False -- ???

missileSize :: Word8 -> Int
missileSize nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

chooseColour :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> TypedIndex Word8
chooseColour True   _    _     True  _     _     _     _     _       = colupf
chooseColour True  True  True  False _     _     _     _     pixelx = if pixelx < 80 then colup0 else colup1
chooseColour True  False True  False _     _     _     _     _       = colupf
chooseColour True  _     False False True  _     _     _     _       = colup0
chooseColour True  _     False False _     _     True  _     _       = colup0
chooseColour True  _     False False False True  False _     _       = colup1
chooseColour True  _     False False False _     False True  _       = colup1
chooseColour True  _     False False False False False False _       = colubk
chooseColour False _     _     _     True  _     _     _     _       = colup0
chooseColour False _     _     _     _     _     True  _     _       = colup0
chooseColour False _     _     _     False True  False _     _       = colup1
chooseColour False _     _     _     False _     False True  _       = colup1
chooseColour False True  True  _     False False False False pixelx = if pixelx < 80 then colup0 else colup1
chooseColour False False True  _     False False False False _       = colupf
chooseColour False _     False True  False False False False _       = colupf
chooseColour False _     False False False False False False _       = colubk

{-# INLINE bit #-}
bit :: Int -> Bool -> Word8
bit n t = if t then 1 `shift` n else 0

doCollisions :: Segment Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
doCollisions word8r lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1 = do
    let playball = bit 7 lplayfield .|. bit 6 lball
    when lmissile0 $ do
        modify cxm0p $ (.|. bit 7 lplayer1 .|. bit 6 lplayer0)
        fastOrIRegister word8r cxm0fb playball
        fastOrIRegister word8r cxppmm $ bit 6 lmissile1
    when lmissile1 $ do
        fastOrIRegister word8r cxm1p $ bit 7 lplayer0 .|. bit 6 lplayer1
        fastOrIRegister word8r cxm1fb playball
    when lplayer0 $ do
        fastOrIRegister word8r cxp0fb playball
        fastOrIRegister word8r cxppmm $ bit 7 lplayer1
    when lplayer1 $ fastOrIRegister word8r cxp1fb playball
    when lball $ fastOrIRegister word8r cxblpf $ bit 7 lplayfield

{- INLINE compositeAndCollide -}
compositeAndCollide :: Segment Word8 -> Segment Int -> Segment Word64 -> Segment Bool -> IRegArray -> Int -> Int -> ORegArray -> IO Word8
compositeAndCollide word8r intr word64r boolr ir pixelx hpos' r = do
    ppos0' <- ld intr s_ppos0
    ppos1' <- ld intr s_ppos1
    mpos0' <- ld intr s_mpos0
    mpos1' <- ld intr s_mpos1
    bpos' <- ld intr s_bpos

    resmp0' <- fastGetORegister r resmp0
    resmp1' <- fastGetORegister r resmp1
    ctrlpf' <- fastGetORegister r ctrlpf
    enam0' <- fastGetORegister r enam0
    enam1' <- fastGetORegister r enam1
    nusiz0' <- fastGetORegister r nusiz0
    nusiz1' <- fastGetORegister r nusiz1
    delayP0' <- ld boolr delayP0
    delayP1' <- ld boolr delayP1
    delayBall' <- ld boolr delayBall
    oldBall' <- ld boolr oldBall
    newBall' <- ld boolr newBall
    pf' <- ld word64r pf

    let lmissile0 = missile nusiz0' enam0' (hpos'-mpos0') resmp0'
    let lmissile1 = missile nusiz1' enam1' (hpos'-mpos1') resmp1'
    lplayer0 <- player0 word8r r delayP0' nusiz0' (hpos'-ppos0')
    lplayer1 <- player1 word8r r delayP1' nusiz1' (hpos'-ppos1')
    let lball = ball delayBall' oldBall' newBall' ctrlpf' (hpos'-bpos')
    let playfieldx = fromIntegral (pixelx `shift` (-2))
    let lplayfield = playfieldx >= 0 && playfieldx < 40 && testBit pf' playfieldx

    doCollisions ir lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1

    fastGetORegister r $ chooseColour (testBit ctrlpf' 2)
                                      (testBit ctrlpf' 1)
                                      lplayfield lball
                                      lmissile0 lmissile1
                                      lplayer0 lplayer1 pixelx

stellaTick :: Int -> Segment Word8 -> Segment Word64 -> Segment Int -> Segment Bool -> IRegArray -> ORegArray -> DebugState -> Ptr Word32 -> IO DebugState
stellaTick n _ _ _ _ _ _ debugState' _ | n <= 0 = return debugState'
stellaTick n word8r word64r intr boolr ir or stellaDebug'@(DebugState { _posbreak = posbreak'@(xbreak', ybreak')}) ptr' = do
    hpos' <- ld intr hpos
    vpos' <- ld intr vpos
    let posbreak'' = if (hpos', vpos') == (xbreak', ybreak') then (-1, -1) else posbreak'

    when (vpos' >= picy && vpos' < picy+screenScanLines && hpos' >= picx) $ do
        -- ptr <- liftIO $ surfacePixels surface
        -- let ptr' = castPtr ptr :: Ptr Word32
        let pixelx = hpos'-picx
        let pixely = vpos'-picy

        let pixelAddr = fromIntegral (screenWidth*pixely+pixelx)

        liftIO $ do
            blank <- fastGetORegister or vblank
            if testBit blank 1
                then pokeElemOff ptr' pixelAddr 0x404040
                else do
                    final <- compositeAndCollide word8r intr word64r boolr ir pixelx hpos' or
                    let rgb = lut!(final `shift` (-1))
                    pokeElemOff ptr' pixelAddr rgb

    let (hpos'', vpos'') = updatePos (hpos', vpos')
    st intr hpos hpos''
    st intr vpos vpos''
    stellaTick (n-1) word8r word64r intr boolr ir or stellaDebug' { _posbreak = posbreak'' } ptr'
