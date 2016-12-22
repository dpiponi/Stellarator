{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}

module VideoOps(clampMissiles,
                stellaTick,
                bit) where

import Data.Word
import Data.Bits hiding (bit) -- XXX check this
import Atari2600
import Control.Monad
import Control.Monad.Trans
import Data.Int
import Prelude hiding (mod)
import Data.Array.Unboxed
import Stella.TIARegisters
import DebugState
import Foreign.Ptr
import Foreign.Storable
import TIAColors
import Metrics

{-# INLINABLE updatePos #-}
updatePos :: (Int, Int) -> (Int, Int)
updatePos (hpos0, vpos0) =
    let hpos' = hpos0+1
    in if hpos' < picx+160
        then (hpos', vpos0)
        else let vpos' = vpos0+1
             in (0, vpos')

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True idx = idx
flipIf False idx = 7-idx

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
stretchPlayer' _       _     _ _      = error "Impossible"

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
missile nusiz0' _      o _                         = o < missileSize nusiz0'

-- Atari2600 programmer's guide p.40
{- INLINE player0 -}
player0 :: Bool -> Word8 -> Int -> MonadAtari Bool
player0 _ _ o | o < 0 = return False
player0 delayP0' nusiz0' o = do
    let sizeCopies = 0b111 .&. nusiz0'
    grp0' <- if delayP0' then load oldGrp0 else load newGrp0
    refp0' <- load refp0
    return $ stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: Bool -> Word8 -> Int -> MonadAtari Bool
player1 _ _ o | o < 0 = return False
player1 delayP1' nusiz1' o = do
    let sizeCopies = 0b111 .&. nusiz1'
    grp1' <- if delayP1' then load oldGrp1 else load newGrp1
    refp1' <- load refp1
    return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

{- INLINE ball -}
ball :: Bool -> Bool -> Bool -> Word8 -> Int -> Bool
ball _ _ _ _ o | o < 0 = False
ball delayBall' oldBall' newBall' ctrlpf' o = do
    let enabl' = if delayBall' then oldBall' else newBall'
    if enabl'
        then do
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (-4)) .&. 0b11))
            o >= 0 && o < ballSize
        else False

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

doCollisions :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> MonadAtari ()
doCollisions lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1 = do
    let playball = bit 7 lplayfield .|. bit 6 lball
    when lmissile0 $ do
        modify cxm0p (.|. (bit 7 lplayer1 .|. bit 6 lplayer0))
        modify cxm0fb (.|. playball)
        modify cxppmm (.|. (bit 6 lmissile1))
    when lmissile1 $ do
        modify cxm1p (.|. (bit 7 lplayer0 .|. bit 6 lplayer1))
        modify cxm1fb (.|. playball)
    when lplayer0 $ do
        modify cxp0fb (.|. playball)
        modify cxppmm (.|. bit 7 lplayer1)
    when lplayer1 $ modify cxp1fb (.|. playball)
    when lball $ modify cxblpf (.|. bit 7 lplayfield)

{- INLINE compositeAndCollide -}
compositeAndCollide :: Int -> Int -> MonadAtari Word8
compositeAndCollide pixelx hpos' = do
    ppos0' <- load s_ppos0
    ppos1' <- load s_ppos1
    mpos0' <- load s_mpos0
    mpos1' <- load s_mpos1
    bpos' <- load s_bpos

    resmp0' <- load resmp0
    resmp1' <- load resmp1
    ctrlpf' <- load ctrlpf
    enam0' <- load enam0
    enam1' <- load enam1
    nusiz0' <- load nusiz0
    nusiz1' <- load nusiz1
    delayP0' <- load delayP0
    delayP1' <- load delayP1
    delayBall' <- load delayBall
    oldBall' <- load oldBall
    newBall' <- load newBall
    pf' <- load pf

    let lmissile0 = missile nusiz0' enam0' (hpos'-mpos0') resmp0'
    let lmissile1 = missile nusiz1' enam1' (hpos'-mpos1') resmp1'
    lplayer0 <- player0 delayP0' nusiz0' (hpos'-ppos0')
    lplayer1 <- player1 delayP1' nusiz1' (hpos'-ppos1')
    let lball = ball delayBall' oldBall' newBall' ctrlpf' (hpos'-bpos')
    let playfieldx = fromIntegral (pixelx `shift` (-2))
    let lplayfield = playfieldx >= 0 && playfieldx < 40 && testBit pf' playfieldx

    doCollisions lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1

    load $ chooseColour (testBit ctrlpf' 2)
                                      (testBit ctrlpf' 1)
                                      lplayfield lball
                                      lmissile0 lmissile1
                                      lplayer0 lplayer1 pixelx

stellaTick :: Int -> DebugState -> Ptr Word32 -> MonadAtari DebugState
stellaTick n debugState' _ | n <= 0 = return debugState'
stellaTick n stellaDebug'@(DebugState { _posbreak = posbreak'@(xbreak', ybreak')}) ptr' = do
    hpos' <- load hpos
    vpos' <- load vpos
    let posbreak'' = if (hpos', vpos') == (xbreak', ybreak') then (-1, -1) else posbreak'

    when (vpos' >= picy && vpos' < picy+screenScanLines && hpos' >= picx) $ do
        let pixelx = hpos'-picx
        let pixely = vpos'-picy

        let pixelAddr = fromIntegral (screenWidth*pixely+pixelx)

        do
            blank <- load vblank
            if testBit blank 1
                then liftIO $ pokeElemOff ptr' pixelAddr 0x404040
                else do
                    final <- compositeAndCollide pixelx hpos'
                    let rgb = lut!(final `shift` (-1))
                    liftIO $ pokeElemOff ptr' pixelAddr rgb

    let (hpos'', vpos'') = updatePos (hpos', vpos')
    store hpos hpos''
    store vpos vpos''
    stellaTick (n-1) stellaDebug' { _posbreak = posbreak'' } ptr'
