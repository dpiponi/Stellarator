{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Strict #-}

module VideoOps(clampMissiles,
                stellaTick,
                clockMove,
                dumpStella,
                bit) where

import Data.Word
import Data.Bits hiding (bit) -- XXX check this
import AcornAtom
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Bits.Lens
import Data.Int
import Prelude hiding (mod)
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

dumpStella :: MonadAcorn ()
dumpStella = do
    liftIO $ putStrLn "--------"
    hpos' <- load hpos
    vpos' <- load vpos
    liftIO $ putStrLn $ "hpos = " ++ show hpos' ++ " (" ++ show (hpos'-picx) ++ ") vpos = " ++ show vpos' ++ " (" ++ show (vpos'-picy) ++ ")"
    grp0' <- load oldGrp0
    grp1' <- load oldGrp1
    refp0' <- load refp0
    refp1' <- load refp1
    liftIO $ putStrLn $ "GRP0 = " ++ showHex grp0' "" ++ "(" ++ inBinary 8 grp0' ++ ")"
    liftIO $ putStrLn $ "GRP1 = " ++ showHex grp1' "" ++ "(" ++ inBinary 8 grp1' ++ ")"
    liftIO $ putStrLn $ "REFP0 = " ++ (if testBit refp0' 3 then "reflected" else " not reflected.")
    liftIO $ putStrLn $ "REFP1 = " ++ (if testBit refp1' 3 then "reflected" else " not reflected.")
    ctrlpf' <- load ctrlpf
    liftIO $ putStrLn $ "CTRLPF = " ++ showHex ctrlpf' "" ++ ": " ++
                        (if testBit ctrlpf' 0 then "reflected" else "not reflected") ++ ", " ++
                        (if testBit ctrlpf' 1 then "score mode" else "not score mode") ++ ", " ++
                        (if testBit ctrlpf' 2 then "playfield priority" else "player priority")
    liftIO $ putStrLn $ "ball size = " ++ show (1 `shift` fromIntegral ((ctrlpf' `shift` (-4)) .&. 3) :: Int)
    {-
    liftIO $ putStrLn $ "PF = " ++ reverse (inBinary 4 (pf0' `shift` (-4)))
                                ++ inBinary 8 pf1'
                                ++ reverse (inBinary 8 pf2')
                                -}
    pf0' <- load pf0
    pf1' <- load pf1
    pf2' <- load pf2
    liftIO $ putStrLn $ "PF0 = " ++ inBinary 8 pf0'
    liftIO $ putStrLn $ "PF1 = " ++ inBinary 8 pf1'
    liftIO $ putStrLn $ "PF2 = " ++ inBinary 8 pf2'
    pf' <- load pf
    liftIO $ putStrLn $ "PF = " ++ inBinary 40 pf'
    pcPf0' <- load pcPf0
    pcPf1' <- load pcPf1
    pcPf2' <- load pcPf2
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcPf0' "" ++ ", " ++ showHex pcPf1' "" ++ ", " ++ showHex pcPf2' ""
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
    mpos0' <- load mpos0
    mpos1' <- load mpos1
    ppos0' <- load ppos0
    ppos1' <- load ppos1
    bpos' <- load bpos
    hmm0' <- load hmm0
    hmm1' <- load hmm1
    hmp0' <- load hmp0
    hmp1' <- load hmp1
    hmbl' <- load hmbl
    pcResp0' <- load pcResp0
    pcResp1' <- load pcResp1
    pcResm0' <- load pcResm0
    pcResm1' <- load pcResm1
    pcResbl' <- load pcResbl
    liftIO $ putStr $ "missile0 @ " ++ show mpos0' ++ "(" ++ show (clockMove hmm0') ++ ")"
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcResm0' ""
    liftIO $ putStr $ "missile1 @ " ++ show mpos1' ++ "(" ++ show (clockMove hmm1') ++ ")"
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcResm1' ""
    liftIO $ putStr $ "player0 @ " ++ show ppos0' ++ "(" ++ show (clockMove hmp0') ++ ")"
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcResp0' ""
    liftIO $ putStr $ "player1 @ " ++ show ppos1' ++ "(" ++ show (clockMove hmp1') ++ ")"
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcResp1' ""
    liftIO $ putStr $ "ball @ " ++ show bpos' ++ "(" ++ show (clockMove hmbl') ++ ")"
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcResbl' ""
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
    pcColubk' <- load pcColubk
    pcColupf' <- load pcColupf
    pcColup0' <- load pcColup0
    pcColup1' <- load pcColup1
    liftIO $ putStrLn $ "COLUBK = " ++ showHex colubk' "" ++ " " ++ colorName colubk'
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcColubk' ""
    liftIO $ putStrLn $ "COLUPF = " ++ showHex colupf' "" ++ " " ++ colorName colupf'
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcColupf' ""
    liftIO $ putStrLn $ "COLUP0 = " ++ showHex colup0' "" ++ " " ++ colorName colup0'
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcColup0' ""
    liftIO $ putStrLn $ "COLUP1 = " ++ showHex colup1' "" ++ " " ++ colorName colup1'
    liftIO $ putStrLn $ " set at address 0x" ++ showHex pcColup1' ""

{-# INLINABLE updatePos #-}
updatePos :: Int -> Int -> (Int, Int)
updatePos hpos0 vpos0 | hpos0 < picx+159 = (hpos0+1, vpos0)
updatePos _     vpos0                    = (0,       vpos0+1)

{-# INLINE flipIf #-}
flipIf :: Bool -> Int -> Int
flipIf True  idx = idx
flipIf False idx = 7-idx

testReflectedBit :: Word8 -> Bool -> Int -> Bool
testReflectedBit bitmap reflect o = testBit bitmap (flipIf reflect $ fromIntegral (o .&. 0x07))

{-
 - See http://atarihq.com/danb/files/stella.pdf page 39
 -}
{- INLINE stretchPlayer' -}
stretchPlayer' :: Bool -> Word8 -> Int -> Word8 -> Bool
stretchPlayer' reflect 0b000 o bitmap = o < 8 && testReflectedBit bitmap reflect o
stretchPlayer' reflect 0b001 o bitmap = (o < 8 || o >= 16 && o < 24) && testReflectedBit bitmap reflect o
stretchPlayer' reflect 0b010 o bitmap = (o < 8 || o >= 32 && o < 40) && testReflectedBit bitmap reflect o
stretchPlayer' reflect 0b011 o bitmap = (o < 8 || o >= 16 && o < 24 || o >= 32 && o < 40) && testReflectedBit bitmap reflect o
stretchPlayer' reflect 0b100 o bitmap = (o < 8 || o >= 64) && testReflectedBit bitmap reflect o
stretchPlayer' reflect 0b101 o bitmap = o < 16 && testReflectedBit bitmap reflect (o `shift` (-1))
stretchPlayer' reflect 0b110 o bitmap = (o < 8 || o >= 32 && o < 40 || o >= 64) && testReflectedBit bitmap reflect o
stretchPlayer' reflect 0b111 o bitmap = o < 32 && testReflectedBit bitmap reflect (o `shift` (-2))
stretchPlayer' _       _     _ _      = error "Impossible"

{- INLINE stretchPlayer -}
stretchPlayer :: Bool -> Int -> Word8 -> Word8 -> Bool
stretchPlayer _       o _          _      | o < 0 || o >= 72 = False
stretchPlayer reflect o sizeCopies bitmap = stretchPlayer' reflect sizeCopies o bitmap

clampMissiles :: Word8 -> Word8 -> MonadAcorn ()
clampMissiles resmp0' resmp1' = do
    when (testBit resmp0' 1) $ ppos0 @-> mpos0
    when (testBit resmp1' 1) $ ppos1 @-> mpos1

-- AcornAtom programmer's guide p.22
{- INLINE missile0 -}
missile :: Word8 -> Word8 -> Int -> Word8 -> Bool
missile _       _      o _       | o < 0                  = False
missile _       _      _ resmp0' | testBit resmp0' 1      = False
missile _       enam0' _ _       | not (testBit enam0' 1) = False
missile nusiz0' _      o _                                = o < missileSize nusiz0'

-- AcornAtom programmer's guide p.40
-- Also
-- http://atariage.com/forums/topic/260569-timing-weirdness-in-battlezone-radar/
{- INLINE player0 -}
player0 :: Int -> Bool -> Word8 -> MonadAcorn Bool
player0 _ _ o | o < 0 = return False
player0 o delayP0' nusiz0' = do
    let sizeCopies = 0b111 .&. nusiz0'
    grp0' <- load $ if delayP0' then oldGrp0 else newGrp0
    refp0' <- load refp0
    return $ stretchPlayer (testBit refp0' 3) (o-if nusiz0' .&. 0x5 == 0x5 then 1 else 0) sizeCopies grp0'

{- INLINE player1 -}
player1 :: Int -> Bool -> Word8 -> MonadAcorn Bool
player1 _ _ o | o < 0 = return False
player1 o delayP1' nusiz1' = do
    let sizeCopies = 0b111 .&. nusiz1'
    grp1' <- load $ if delayP1' then oldGrp1 else newGrp1
    refp1' <- load refp1
    return $ stretchPlayer (testBit refp1' 3) (o-if nusiz1' .&. 0x5 == 0x5 then 1 else 0) sizeCopies grp1'

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

data Sprite = COLUBK | COLUB | COLUPF | COLUP0 | COLUP1 | COLUM0 | COLUM1 deriving (Eq, Show)

spriteColour :: Sprite -> TypedIndex Word8
spriteColour COLUBK = colubk
spriteColour COLUPF = colupf
spriteColour COLUB  = colupf
spriteColour COLUP0 = colup0
spriteColour COLUP1 = colup1
spriteColour COLUM0 = colup0
spriteColour COLUM1 = colup1

debugColour :: Sprite -> Word8
debugColour COLUBK = 0x00 -- black
debugColour COLUPF = 0x0e -- white
debugColour COLUB  = 0x08 -- gray
debugColour COLUP0 = 0x42 -- dark red
debugColour COLUP1 = 0x82 -- dark blue
debugColour COLUM0 = 0x4e -- light red
debugColour COLUM1 = 0x8e -- light blue

--
--           Pri   sco   pf    ball  m0    m1   p0     p1    pixelx
chooseColour :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Sprite
chooseColour True   _    _     True  _     _     _     _     _       = COLUB
chooseColour True  True  True  False _     _     _     _     pixelx  = if pixelx < 80 then COLUP0 else COLUP1
chooseColour True  False True  False _     _     _     _     _       = COLUPF
chooseColour True  _     False False True  _     _     _     _       = COLUM0
chooseColour True  _     False False _     _     True  _     _       = COLUP0
chooseColour True  _     False False False True  False _     _       = COLUM1
chooseColour True  _     False False False _     False True  _       = COLUP1
chooseColour True  _     False False False False False False _       = COLUBK
chooseColour False _     _     _     True  _     _     _     _       = COLUM0
chooseColour False _     _     _     _     _     True  _     _       = COLUP0
chooseColour False _     _     _     False True  False _     _       = COLUM1
chooseColour False _     _     _     False _     False True  _       = COLUP1
chooseColour False True  True  _     False False False False pixelx  = if pixelx < 80 then COLUP0 else COLUP1
chooseColour False False True  _     False False False False _       = COLUPF
chooseColour False _     False True  False False False False _       = COLUB
chooseColour False _     False False False False False False _       = COLUBK

{-# INLINE bit #-}
bit :: Int -> Bool -> Word8
bit _ False = 0
bit n True  = 1 `shift` n

--
-- Build matrix of 6C2 possible collisions between 6 sprites. XXX incomplete!
--
-- 6-bit Address Address Name  7   6   5   4   3   2   1   0  Function    D7    D6
-- 0             CXM0P         1   1   .   .   .   .   .   .  read collision    M0 P1    M0 P0
-- 1             CXM1P         1   1   .   .   .   .   .   .  read collision    M1 P0    M1 P1
-- 2             CXP0FB        1   1   .   .   .   .   .   .  read collision    P0 PF    P0 BL
-- 3             CXP1FB        1   1   .   .   .   .   .   .  read collision    P1 PF    P1 BL
-- 4             CXM0FB        1   1   .   .   .   .   .   .  read collision    M0 PF    M0 BL
-- 5             CXM1FB        1   1   .   .   .   .   .   .  read collision    M1 PF    M1 BL
-- 6             CXBLPF        1   .   .   .   .   .   .   .  read collision    BL PF    unused
-- 7             CXPPMM        1   1   .   .   .   .   .   .  read collision    P0 P1    M0 M1
doCollisions :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> MonadAcorn ()
doCollisions lplayfield lball lmissile0 lmissile1 lplayer0 lplayer1 = do
    -- Combine playfield and ball into single byte
    -- Remember `&` is reverse function application, not logical AND
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

wrap160' :: Int -> Int
wrap160' x' | x' < 0 = x'+160
wrap160' x' = x'

{- INLINE compositeAndCollide -}
compositeAndCollide :: Int -> Int -> MonadAcorn Word8
compositeAndCollide pixelx hpos' = do
    ppos0' <- load ppos0
    ppos1' <- load ppos1
    mpos0' <- load mpos0
    mpos1' <- load mpos1
    bpos' <- load bpos

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
    let sprite = chooseColour playfieldPriority
                                scoreMode
                                lplayfield lball
                                lmissile0 lmissile1
                                lplayer0 lplayer1 pixelx
    debugColours' <- load debugColours
    z <- if debugColours'
            then return $ debugColour sprite
            else load $ spriteColour sprite

    vpos' <- load vpos
    if vpos' == 174 && hpos' == 175
    then do
        return z
    else return z

{-# INLINE stellaTick #-}
stellaTick :: Int -> Ptr Word8 -> MonadAcorn ()
stellaTick n _ | n <= 0 = return ()
stellaTick n ptr' = do
    hpos' <- load hpos
    vpos' <- load vpos
    xbreak' <- load xbreak
    ybreak' <- load ybreak
    when ((hpos', vpos') == (xbreak', ybreak')) $ do
        dumpStella
        xbreak @= (-1)
        ybreak @= (-1)

    when (vpos' >= picy && vpos' < picy+screenScanLines && hpos' >= picx) $ do
        let pixelx = hpos'-picx
        let pixely = vpos'-picy

        let pixelAddr = fromIntegral (screenWidth*pixely+pixelx)

        blank <- load vblank
        pendingHmove' <- load pendingHmove
        let renderBlank = testBit blank 1 || pendingHmove' && pixelx < 8

        -- This is the place where the final pixel goes into
        -- the frame buffer
        pixel <- if renderBlank
            then return 0
            else compositeAndCollide pixelx hpos'
        liftIO $ pokeElemOff ptr' pixelAddr pixel

        when (pixelx >= 8) $ pendingHmove @= False

    let (hpos'', vpos'') = updatePos hpos' vpos'
    hpos @= hpos''
    vpos @= vpos''
    stellaTick (n-1) ptr'
