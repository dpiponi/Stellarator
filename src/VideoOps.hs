{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}

module VideoOps(compositeAndCollide,
                clampMissiles,
                stellaTick,
                bit) where

import Data.Word
import Data.Bits hiding (bit) -- XXX check this
import Atari2600
import Stella.Sprites
import Control.Monad
import Control.Monad.Trans
import Data.Array.Unboxed
import Data.Array.IO
import Stella.TIARegisters
import Control.Lens
import Stella.Graphics
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

clampMissiles :: Word8 -> Word8 -> Sprites -> Sprites
clampMissiles resmp0' resmp1' (sprites'@Sprites { _s_ppos0 = ppos0',
                                                  _s_ppos1 = ppos1',
                                                  _s_mpos0 = mpos0',
                                                  _s_mpos1 = mpos1' }) =
    let mpos0'' =  if testBit resmp0' 1 then ppos0' else mpos0'
        mpos1'' =  if testBit resmp1' 1 then ppos1' else mpos1'
    in sprites' { _s_mpos0 = mpos0'', _s_mpos1 = mpos1'' }

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
player0 :: IOUArray OReg Word8 -> Graphics -> Word8 -> Int -> IO Bool
player0 _ _ _ o | o < 0 = return False
player0 r graphics'@(Graphics {_delayP0 = delayP0',
                               _oldGrp0 = oldGrp0',
                               _newGrp0 = newGrp0'}) nusiz0' o = do
    let sizeCopies = 0b111 .&. nusiz0'
    let grp0' = if delayP0' then oldGrp0' else newGrp0'
    refp0' <- fastGetORegister r refp0
    return $ stretchPlayer (testBit refp0' 3) sizeCopies o grp0'

{- INLINE player1 -}
player1 :: IOUArray OReg Word8 -> Graphics -> Word8 -> Int -> IO Bool
player1 _ _ _ o | o < 0 = return False
player1 r (Graphics {_delayP1 = delayP1',
                     _oldGrp1 = oldGrp1',
                     _newGrp1 = newGrp1'}) nusiz1' o = do
    let sizeCopies = 0b111 .&. nusiz1'
    let grp1' = if delayP1' then oldGrp1' else newGrp1'
    refp1' <- fastGetORegister r refp1
    return $ stretchPlayer (testBit refp1' 3) sizeCopies o grp1'

{- INLINE ball -}
ball :: Graphics -> Word8 -> Int -> Bool
ball _ _ o | o < 0 = False
ball (Graphics { _delayBall = delayBall',
                 _oldBall = oldBall',
                 _newBall = newBall'}) ctrlpf' o = do
    let enabl' = if delayBall' then oldBall' else newBall'
    if enabl'
        then do
            let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (fromIntegral $ -4)) .&. 0b11))
            o >= 0 && o < ballSize
        else False

{- INLINE playfield -}
playfield :: IOUArray OReg Word8 -> Word8 -> Int -> IO Bool
playfield r ctrlpf' i | i >= 0  && i < 4  = flip testBit (i+4)  <$> fastGetORegister r pf0
                      | i >=4   && i < 12 = flip testBit (11-i) <$> fastGetORegister r pf1
                      | i >= 12 && i < 20 = flip testBit (i-12) <$> fastGetORegister r pf2
playfield r ctrlpf' i | i >= 20 && i < 40 = playfield r ctrlpf' $ if testBit ctrlpf' 0 then 39-i else i-20
playfield _ _ _ = return $ False -- ???

missileSize :: Word8 -> Int
missileSize nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

chooseColour :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> OReg
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

doCollisions :: IOUArray IReg Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
doCollisions ir lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1 = do
    let playball = bit 7 lplayfield .|. bit 6 lball
    when lmissile0 $ do
        fastOrIRegister ir cxm0p $ bit 7 lplayer1 .|. bit 6 lplayer0
        fastOrIRegister ir cxm0fb playball
        fastOrIRegister ir cxppmm $ bit 6 lmissile1
    when lmissile1 $ do
        fastOrIRegister ir cxm1p $ bit 7 lplayer0 .|. bit 6 lplayer1
        fastOrIRegister ir cxm1fb playball
    when lplayer0 $ do
        fastOrIRegister ir cxp0fb playball
        fastOrIRegister ir cxppmm $ bit 7 lplayer1
    when lplayer1 $ fastOrIRegister ir cxp1fb playball
    when lball $ fastOrIRegister ir cxblpf $ bit 7 lplayfield

{- INLINE compositeAndCollide -}
compositeAndCollide :: Hardware -> IRegArray -> Graphics -> Sprites -> Int -> Int -> ORegArray -> IO Word8
compositeAndCollide hardware' ir graphics' sprites' pixelx hpos' r = do
    let Sprites { _s_mpos0 = mpos0',
              _s_mpos1 = mpos1',
              _s_ppos0 = ppos0',
              _s_ppos1 = ppos1',
              _s_bpos  = bpos'
            } = sprites'
    let pf' = hardware' ^. pf
    -- let ir = hardware' ^. iregisters

    resmp0' <- fastGetORegister r resmp0
    resmp1' <- fastGetORegister r resmp1
    ctrlpf' <- fastGetORegister r ctrlpf
    enam0' <- fastGetORegister r enam0
    enam1' <- fastGetORegister r enam1
    nusiz0' <- fastGetORegister r nusiz0
    nusiz1' <- fastGetORegister r nusiz1

    let lmissile0 = missile nusiz0' enam0' (hpos'-mpos0') resmp0'
    let lmissile1 = missile nusiz1' enam1' (hpos'-mpos1') resmp1'
    lplayer0 <- player0 r graphics' nusiz0' (hpos'-ppos0')
    lplayer1 <- player1 r graphics' nusiz1' (hpos'-ppos1')
    let lball = ball graphics' ctrlpf' (hpos'-bpos')
    let playfieldx = fromIntegral (pixelx `shift` (-2))
    let lplayfield = playfieldx >= 0 && playfieldx < 40 && testBit pf' playfieldx

    doCollisions ir lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1

    fastGetORegister r $ chooseColour (testBit ctrlpf' 2)
                                      (testBit ctrlpf' 1)
                                      lplayfield lball
                                      lmissile0 lmissile1
                                      lplayer0 lplayer1 pixelx

stellaTick :: Int -> IRegArray -> ORegArray -> Hardware -> Graphics -> Sprites -> Ptr Word32 -> IO Hardware
stellaTick n _ _ hardware' _ _ _ | n <= 0 = return hardware'
stellaTick n ir or hardware'@(Hardware { _stellaDebug = stellaDebug'@(DebugState { _posbreak = posbreak'@(xbreak', ybreak')}),
                                         _position = position'@(hpos', vpos') }) graphics' sprites' ptr' = do
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
                    final <- compositeAndCollide hardware' ir graphics' sprites' pixelx hpos' or
                    let rgb = lut!(final `shift` (-1))
                    pokeElemOff ptr' pixelAddr rgb

    stellaTick (n-1) ir or hardware' { _position = updatePos position',
                                   _stellaDebug = stellaDebug' { _posbreak = posbreak'' } } graphics' sprites' ptr'
