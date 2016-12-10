module Debugger where

import qualified Data.Map.Strict as Map

import Core
import Atari2600
import DebugCmd
import Data.Bits
import DebugState
import Control.Monad.State.Strict
import Control.Lens

comparison :: (Int -> Int -> Bool) -> Expr -> Expr -> MonadAtari Value
comparison op x y = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EBool (x `op` y)
            otherwise -> return EFail

arith :: (Int -> Int -> Int) -> Expr -> Expr -> MonadAtari Value
arith op x y = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x `op` y)
            otherwise -> return EFail

eval :: Expr -> MonadAtari Value
eval A = do
    a <- getA
    return (EInt (fromIntegral a))
eval X = do
    x <- getX
    return (EInt (fromIntegral x))
eval Y = do
    y <- getY
    return (EInt (fromIntegral y))
eval PC = do
    pc <- getPC
    return (EInt (fromIntegral pc))
eval DebugCmd.S = do
    s <- getS
    return (EInt (fromIntegral s))
eval DebugCmd.EQ = do
    z <- getZ
    return (EBool z)
eval NE = do
    z <- getZ
    return (EBool (not z))
eval CC = do
    c <- getC
    return (EBool c)
eval CS = do
    c <- getC
    return (EBool (not c))
eval PL = do
    n <- getN
    return (EBool (not n))
eval MI = do
    n <- getN
    return (EBool n)
eval DebugCmd.Clock = do
    n <- use clock
    return (EInt (fromIntegral n))
eval Row = do
    n <- use vpos
    return (EInt (fromIntegral n))
eval Col = do
    n <- use hpos
    return (EInt (fromIntegral n))

eval (Var s) = do
    v <- use (hardware . stellaDebug . variables)
    case Map.lookup s v of
        Nothing -> return EFail
        Just x -> return x

eval (Or x y) = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x .|. y)
            (EBool x, EBool y) -> return $ EBool (x || y)
            otherwise -> return EFail

eval (And x y) = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x .&. y)
            (EBool x, EBool y) -> return $ EBool (x && y)
            otherwise -> return EFail

eval (Gt x y) = comparison (>) x y
eval (Ge x y) = comparison (>=) x y
eval (Le x y) = comparison (<=) x y
eval (Eq x y) = comparison (==) x y
eval (Ne x y) = comparison (/=) x y
eval (Lt x y) = comparison (<) x y
eval (Plus x y) = arith (+) x y
eval (Times x y) = arith (*) x y
eval (Div x y) = arith div x y
eval (Minus x y) = arith (-) x y
eval (LeftShift x y) = arith (shift) x y
eval (RightShift x y) = arith (shift . negate) x y

eval (PeekByte x) = do
        x' <- eval x
        case x' of
            EInt x -> do
                y <- readMemory (fromIntegral x)
                return (EInt $ fromIntegral y)
            otherwise -> return EFail

eval (PeekWord x) = do
        x' <- eval x
        case x' of
            EInt x -> do
                lo <- readMemory (fromIntegral x)
                hi <- readMemory (fromIntegral x+1)
                return (EInt $ fromIntegral $ Core.make16 lo hi)
            otherwise -> return EFail

eval (Not x) = do
        x' <- eval x
        case x' of
            EBool x -> return $ EBool (not x)
            EInt x -> return $ EInt (-1-x)
            otherwise -> return EFail

eval (EConst x) = return (EInt x)
eval (EConstString s) = return (EString s)

eval x = do
    liftIO $ print x
    return EFail
