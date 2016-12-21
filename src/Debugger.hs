module Debugger(runDebugger) where

import qualified Data.Map.Strict as Map

import Core
import Atari2600
import Emulation
import Disasm
import DebugCmd
import Data.Bits
import DebugState
import Control.Monad
import Text.Parsec
import Control.Monad.State.Strict
import Control.Lens
import System.Console.Haskeline
import Stella.TIARegisters

comparison :: (Int -> Int -> Bool) -> Expr -> Expr -> MonadAtari Value
comparison operator expr0 expr1 = do
        value0 <- eval expr0
        value1 <- eval expr1
        case (value0, value1) of
            (EInt x, EInt y) -> return $ EBool (x `operator` y)
            _ -> return EFail

arith :: (Int -> Int -> Int) -> Expr -> Expr -> MonadAtari Value
arith operator expr0 expr1 = do
        x' <- eval expr0
        y' <- eval expr1
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x `operator` y)
            _ -> return EFail

eval :: Expr -> MonadAtari Value
eval A = do
    value <- getA
    return (EInt (fromIntegral value))
eval X = do
    value <- getX
    return (EInt (fromIntegral value))
eval Y = do
    value <- getY
    return (EInt (fromIntegral value))
eval PC = do
    value <- getPC
    return (EInt (fromIntegral value))
eval DebugCmd.S = do
    value <- getS
    return (EInt (fromIntegral value))
eval DebugCmd.EQ = do
    value <- getZ
    return (EBool value)
eval NE = do
    value <- getZ
    return (EBool (not value))
eval CC = do
    value <- getC
    return (EBool value)
eval CS = do
    value <- getC
    return (EBool (not value))
eval PL = do
    value <- getN
    return (EBool (not value))
eval MI = do
    value <- getN
    return (EBool value)
eval DebugCmd.Clock = do
    value <- useClock id
    return (EInt (fromIntegral value))
eval Row = do
    intr <- view intArray
    value <- liftIO $ ld intr vpos
    return (EInt (fromIntegral value))
eval Col = do
    intr <- view intArray
    value <- liftIO $ ld intr hpos
    return (EInt (fromIntegral value))

eval (Var s) = do
    v <- useStellaDebug variables
    case Map.lookup s v of
        Nothing -> return EFail
        Just x -> return x

eval (Or x y) = do
        x' <- eval x
        y' <- eval y
        case (x', y') of
            (EInt x, EInt y) -> return $ EInt (x .|. y)
            (EBool x, EBool y) -> return $ EBool (x || y)
            _ -> return EFail

eval (And expr0 expr1) = do
        value0 <- eval expr0
        value1 <- eval expr1
        case (value0, value1) of
            (EInt x, EInt y) -> return $ EInt (x .&. y)
            (EBool x, EBool y) -> return $ EBool (x && y)
            _ -> return EFail

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

eval (PeekByte expr) = do
        value <- eval expr
        case value of
            EInt x -> do
                y <- readMemory (fromIntegral x)
                return (EInt $ fromIntegral y)
            _ -> return EFail

eval (PeekWord expr) = do
        value <- eval expr
        case value of
            EInt x -> do
                lo <- readMemory (fromIntegral x)
                hi <- readMemory (fromIntegral x+1)
                return (EInt $ fromIntegral $ Core.make16 lo hi)
            _ -> return EFail

eval (Not expr) = do
    value <- eval expr
    case value of
        EBool x -> return $ EBool (not x)
        EInt x -> return $ EInt (-1-x)
        _ -> return EFail

eval (EConst x) = return (EInt x)
eval (EConstString s) = return (EString s)

eval x = do
    liftIO $ print x
    return EFail

disassemble :: Maybe Expr -> Maybe Expr -> MonadAtari ()
disassemble addr n = do
    n' <- case n of
        Just e -> do
            x' <- eval e
            case x' of
                EInt n'' -> return n''
                _ -> return 1
        _ -> return 1
    startPC <- case addr of
        Just exprPC -> do
            x' <- eval exprPC
            case x' of
                EInt z -> return (fromIntegral z)
                _ -> return 0 -- error
        Nothing -> getPC
    bytes <- forM [startPC..startPC+3*fromIntegral n'] readMemory
    liftIO $ dis n' startPC bytes

execCommand :: Command -> MonadAtari Bool
execCommand cmd = 
    case cmd of
        Let var e -> do
            e' <- eval e
            modifyStellaDebug variables $ Map.insert var e'
            _ <- useStellaDebug variables -- Uh? XXX
            return False
        Block cmds -> do
            forM_ cmds execCommand
            return False
        DebugCmd.List addr n -> do
            disassemble addr n
            return False
        Repeat n repeatedCmd -> do
            n' <- eval n
            case n' of
                EInt n'' -> do
                    replicateM_ n'' (execCommand repeatedCmd)
                _ -> return ()
            return False
        Cont -> do
            liftIO $ putStrLn "Continuing..."
            return True
        DumpGraphics -> dumpStella >> return False
        Step -> step >> return False
        Print es -> do
            forM_ es $ \e -> do
                val <- eval e
                liftIO $ putStr (show val)
            liftIO $ putStrLn ""
            return False
        Until cond repeatedCmd -> do
            let loop = (do
                            c <- eval cond
                            case c of
                                EBool True -> return ()
                                EBool False -> do
                                    void $ execCommand repeatedCmd
                                    loop
                                _ -> do
                                    liftIO $ putStrLn "Non-boolean condition"
                                    return ())
            loop
            return False

runDebugger :: MonadAtari ()
runDebugger = do
    Just line <- liftIO $ runInputT (defaultSettings { historyFile=Just ".stellarator" }) $ getInputLine "> "
    let cmd = parse parseCommand "" line
    case cmd of
        Right cmd' -> do
            q <- execCommand cmd'
            when (not q) runDebugger
        Left e -> do
            liftIO $ print e
            runDebugger
