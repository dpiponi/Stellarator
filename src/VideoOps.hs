{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}

module VideoOps(clampMissiles,
                stellaTick,
                clockMove,
                dumpStella,
                bit) where

import Data.Word
import Data.Bits hiding (bit) -- XXX check this
import Atari2600
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Bits.Lens
import Data.Int
import Prelude hiding (mod)
import Data.Array.Unboxed
import Asm
--import DebugState
import Foreign.Ptr
import Foreign.Storable
import TIAColors
import Numeric
import Metrics

explainNusiz :: Word8 -> String
explainNusiz nusiz =
    case nusiz .&. 0b111 of
        0b000 -> "one copy"
        0b001 -> "two copies - close"
        0b010 -> "two copies - med"
        0b011 -> "three copies - close"
        0b100 -> "two copies - wide"
        0b101 -> "double size player"
        0b110 -> "3 copies medium"
        0b111 -> "quad sized player"
        _ -> error "Impossible to reach"

inBinary :: (Bits a) => Int -> a -> String
inBinary 0 _ = ""
inBinary n m = inBinary (n-1) (m `shift` (-1)) ++ if testBit m 0 then "1" else "0"

{-# INLINE clockMove #-}
clockMove :: Word8 -> Int
clockMove i = fromIntegral ((fromIntegral i :: Int8) `shift` (-4))

dumpStella :: MonadAtari ()
dumpStella = do
    liftIO $ putStrLn "--------"
    hpos' <- load hpos
    vpos' <- load vpos
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    grp0' <- load oldGrp0
    grp1' <- load oldGrp1
    liftIO $ putStrLn $ "GRP0 = " ++ showHex grp0' "" ++ "(" ++ inBinary 8 grp0' ++ ")"
    liftIO $ putStrLn $ "GRP1 = " ++ showHex grp1' "" ++ "(" ++ inBinary 8 grp1' ++ ")"
    ctrlpf' <- load ctrlpf
    liftIO $ putStrLn $ "CTRLPF = " ++ showHex ctrlpf' "" ++ ": " ++
                        (if testBit ctrlpf' 0 then "reflected" else "not reflected") ++ ", " ++
                        (if testBit ctrlpf' 1 then "score mode" else "not score mode") ++ ", " ++
                        (if testBit ctrlpf' 2 then "playfield priority" else "player priority")
    liftIO $ putStrLn $ "ball size = " ++ show (1 `shift` fromIntegral ((ctrlpf' `shift` (-4)) .&. 3) :: Int)
    pf0' <- load pf0
    pf1' <- load pf1
    pf2' <- load pf2
    liftIO $ putStrLn $ "PF = " ++ reverse (inBinary 4 (pf0' `shift` (-4)))
                                ++ inBinary 8 pf1'
                                ++ reverse (inBinary 8 pf2')
    pf' <- load pf
    liftIO $ putStrLn $ inBinary 40 pf'
    nusiz0' <- load nusiz0
    nusiz1' <- load nusiz1
    liftIO $ putStrLn $ "NUSIZ0 = " ++ showHex nusiz0' "" ++ "(" ++ explainNusiz nusiz0' ++
                        ") NUSIZ1 = " ++ showHex nusiz1' "" ++ "(" ++ explainNusiz nusiz1' ++ ")"
    enam0' <- load enam0
    enam1' <- load enam1
    enablOld <- load oldBall
    enablNew <- load newBall
    liftIO $ putStr $ "ENAM0 = " ++ show (testBit enam0' 1)
    liftIO $ putStr $ " ENAM1 = " ++ show (testBit enam1' 1)
    liftIO $ putStrLn $ " ENABL = " ++ show (enablOld, enablNew)
    mpos0' <- load s_mpos0
    mpos1' <- load s_mpos1
    ppos0' <- load s_ppos0
    ppos1' <- load s_ppos1
    bpos' <- load s_bpos
    hmm0' <- load hmm0
    hmm1' <- load hmm1
    hmp0' <- load hmp0
    hmp1' <- load hmp1
    hmbl' <- load hmbl
    liftIO $ putStr $ "missile0 @ " ++ show mpos0' ++ "(" ++ show (clockMove hmm0') ++ ")"
    liftIO $ putStrLn $ " missile1 @ " ++ show mpos1' ++ "(" ++ show (clockMove hmm1') ++ ")"
    liftIO $ putStr $ "player0 @ " ++ show ppos0' ++ "(" ++ show (clockMove hmp0') ++ ")"
    liftIO $ putStrLn $ " player1 @ " ++ show ppos1' ++ "(" ++ show (clockMove hmp1') ++ ")"
    liftIO $ putStrLn $ "ball @ " ++ show bpos' ++ "(" ++ show (clockMove hmbl') ++ ")"
    vdelp0' <- load delayP0
    vdelp1' <- load delayP1
    vdelbl' <- load delayBall
    liftIO $ putStrLn $ "VDELP0 = " ++ show vdelp0' ++ " " ++
                        "VDELP1 = " ++ show vdelp1' ++ " " ++
                        "VDELBL = " ++ show vdelbl'
    colubk' <- load colubk
    colupf' <- load colupf
    colup0' <- load colup0
    colup1' <- load colup1
    liftIO $ putStrLn $ "COLUBK = " ++ showHex colubk' "" ++ " " ++ colorName colubk'
    liftIO $ putStrLn $ "COLUPF = " ++ showHex colupf' "" ++ " " ++ colorName colupf'
    liftIO $ putStrLn $ "COLUP0 = " ++ showHex colup0' "" ++ " " ++ colorName colup0'
    liftIO $ putStrLn $ "COLUP1 = " ++ showHex colup1' "" ++ " " ++ colorName colup1'

{-# INLINABLE updatePos #-}
updatePos :: Int -> Int -> (Int, Int)
updatePos hpos0 vpos0 | hpos0 < picx+159 = (hpos0+1, vpos0)
updatePos _     vpos0                    = (0,       vpos0+1)

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True  idx = idx
flipIf False idx = 7-idx

{-
 - See http://atarihq.com/danb/files/stella.pdf page 39
 -}
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
stretchPlayer :: Bool -> Int -> Word8 -> Word8 -> Bool
stretchPlayer _       o _          _      | o < 0 || o >= 72 = False
stretchPlayer reflect o sizeCopies bitmap = stretchPlayer' reflect sizeCopies o bitmap

clampMissiles :: Word8 -> Word8 -> MonadAtari ()
clampMissiles resmp0' resmp1' = do
    when (testBit resmp0' 1) $ load s_ppos0 >>= store s_mpos0
    when (testBit resmp1' 1) $ load s_ppos1 >>= store s_mpos1

-- Atari2600 programmer's guide p.22
{- INLINE missile0 -}
missile :: Word8 -> Word8 -> Int -> Word8 -> Bool
missile _       _      o _       | o < 0                  = False
missile _       _      _ resmp0' | testBit resmp0' 1      = False
missile _       enam0' _ _       | not (testBit enam0' 1) = False
missile nusiz0' _      o _                                = o < missileSize nusiz0'

-- Atari2600 programmer's guide p.40
{- INLINE player0 -}
player0 :: Int -> Bool -> Word8 -> MonadAtari Bool
player0 _ _ o | o < 0 = return False
player0 o delayP0' nusiz0' = do
    let sizeCopies = 0b111 .&. nusiz0'
    grp0' <- load $ if delayP0' then oldGrp0 else newGrp0
    refp0' <- load refp0
    return $ stretchPlayer (testBit refp0' 3) o sizeCopies grp0'

{- INLINE player1 -}
player1 :: Int -> Bool -> Word8 -> MonadAtari Bool
player1 _ _ o | o < 0 = return False
player1 o delayP1' nusiz1' = do
    let sizeCopies = 0b111 .&. nusiz1'
    grp1' <- load $ if delayP1' then oldGrp1 else newGrp1
    refp1' <- load refp1
    return $ stretchPlayer (testBit refp1' 3) o sizeCopies grp1'

{- INLINE ball -}
ball :: Bool -> Bool -> Bool -> Word8 -> Int -> Bool
ball _ _ _ _ o | o < 0 = False
ball delayBall' oldBall' newBall' ctrlpf' o = do
    let enabl' = if delayBall' then oldBall' else newBall'
    if enabl'
        then let ballSize = 1 `shift` (fromIntegral ((ctrlpf' `shift` (-4)) .&. 0b11))
             in o >= 0 && o < ballSize
        else False

missileSize :: Word8 -> Int
missileSize nusiz = 1 `shift` (fromIntegral ((nusiz `shift` (-4)) .&. 0b11))

--
--           Pri   sco   pf    ball  m0    m1   p0     p1    pixelx
chooseColour :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> TypedIndex Word8
chooseColour True   _    _     True  _     _     _     _     _       = colupf
chooseColour True  True  True  False _     _     _     _     pixelx  = if pixelx < 80 then colup0 else colup1
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
chooseColour False True  True  _     False False False False pixelx  = if pixelx < 80 then colup0 else colup1
chooseColour False False True  _     False False False False _       = colupf
chooseColour False _     False True  False False False False _       = colupf
chooseColour False _     False False False False False False _       = colubk

{-# INLINE bit #-}
bit :: Int -> Bool -> Word8
bit _ False = 0
bit n True  = 1 `shift` n

{-
 - Build matrix of 6C2 possible collisions between 6 sprites
 -}
doCollisions :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> MonadAtari ()
doCollisions lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1 = do
    let playball = 0 & bitAt 7 .~ lplayfield & bitAt 6 .~ lball
    when lmissile0 $ do
        modify cxm0p  $ (bitAt 7 ||~ lplayer1) . (bitAt 6 ||~ lplayer0)
        modify cxm0fb (.|. playball)
        modify cxppmm $ bitAt 6 ||~ lmissile1
    when lmissile1 $ do
        modify cxm1p  $ (bitAt 7 ||~ lplayer0) . (bitAt 6 ||~ lplayer1)
        modify cxm1fb (.|. playball)
    when lplayer0  $ do
        modify cxp0fb (.|. playball)
        modify cxppmm $ bitAt 7 ||~ lplayer1
    when lplayer1  $ modify cxp1fb (.|. playball)
    when lball     $ modify cxblpf $ bitAt 7 ||~ lplayfield

wrap160' x | x < 0 = x+160
wrap160' x = x

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

    let lmissile0 = missile nusiz0' enam0' (wrap160' $ hpos'-mpos0') resmp0'
    let lmissile1 = missile nusiz1' enam1' (wrap160' $ hpos'-mpos1') resmp1'
    lplayer0 <- player0 (wrap160' $ hpos'-ppos0') delayP0' nusiz0'
    lplayer1 <- player1 (wrap160' $ hpos'-ppos1') delayP1' nusiz1'
    let lball = ball delayBall' oldBall' newBall' ctrlpf' (wrap160' $ hpos'-bpos')
    let playfieldx = fromIntegral (pixelx `shift` (-2))
    let lplayfield = playfieldx >= 0 && playfieldx < 40 && testBit pf' playfieldx

    doCollisions lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1

    let scoreMode = testBit ctrlpf' 1
    let playfieldPriority = testBit ctrlpf' 2
    z <- load $ chooseColour playfieldPriority
                        scoreMode
                        lplayfield lball
                        lmissile0 lmissile1
                        lplayer0 lplayer1 pixelx
    vpos' <- load vpos
    if vpos' == 174 && hpos' == 175
    then do
        return z
    {-
        liftIO $ print $ "lball = " ++ show lball
        liftIO $ print $ "scoreMode = " ++ show scoreMode
        liftIO $ print $ "z=" ++ show z
        return 0xff
        -}
    else return z

{-# INLINE stellaTick #-}
stellaTick :: Int -> Ptr Word8 -> MonadAtari ()
stellaTick n _ | n <= 0 = return ()
stellaTick n ptr' = do
    hpos' <- load hpos
    vpos' <- load vpos
    xbreak' <- load xbreak
    ybreak' <- load ybreak
    when ((hpos', vpos') == (xbreak', ybreak')) $ do
        dumpStella
        store xbreak (-1)
        store ybreak (-1)

    when (vpos' >= picy && vpos' < picy+screenScanLines && hpos' >= picx) $ do
        let pixelx = hpos'-picx
        let pixely = vpos'-picy

        let pixelAddr = fromIntegral (screenWidth*pixely+pixelx)

        blank <- load vblank
        pendingHmove' <- load pendingHmove
        let renderBlank = testBit blank 1 || pendingHmove' && pixelx < 8
        pixel <- if renderBlank
            then return 0
            else compositeAndCollide pixelx hpos'
        liftIO $ pokeElemOff ptr' pixelAddr pixel

        when (pixelx >= 8) $ store pendingHmove False

    let (hpos'', vpos'') = updatePos hpos' vpos'
    store hpos hpos''
    store vpos vpos''
    stellaTick (n-1) ptr'
