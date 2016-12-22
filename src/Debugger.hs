module Debugger(runDebugger) where

import qualified Data.Map.Strict as Map

import Atari2600
--import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Core
import Data.Bits
import DebugCmd
import DebugState
import Disasm
import Emulation
import Stella.TIARegisters
import System.Console.Haskeline
import Text.Parsec

comparison :: (Int -> Int -> Bool) -> Expr -> Expr -> MonadAtari Value
comparison operator expr0 expr1 = do
        value0 <- eval expr0
        value1 <- eval expr1
        case (value0, value1) of
            (EInt operand0, EInt operand1) -> return $ EBool (operand0 `operator` operand1)
            _ -> return EFail

arith :: (Int -> Int -> Int) -> Expr -> Expr -> MonadAtari Value
arith operator expr0 expr1 = do
        value0 <- eval expr0
        value1 <- eval expr1
        case (value0, value1) of
            (EInt operand0, EInt operand1) -> return $ EInt (operand0 `operator` operand1)
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
    value <- load vpos
    return (EInt (fromIntegral value))
eval Col = do
    value <- load hpos
    return (EInt (fromIntegral value))

eval (Var name) = do
    v <- useStellaDebug variables
    case Map.lookup name v of
        Nothing -> return EFail
        Just value -> return value

eval (Or expr0 expr1) = do
        value0 <- eval expr0
        value1 <- eval expr1
        case (value0, value1) of
            (EInt operand0, EInt operand1) -> return $ EInt (operand0 .|. operand1)
            (EBool operand0, EBool operand1) -> return $ EBool (operand0 || operand1)
            _ -> return EFail

eval (And expr0 expr1) = do
        value0 <- eval expr0
        value1 <- eval expr1
        case (value0, value1) of
            (EInt operand0, EInt operand1) -> return $ EInt (operand0 .&. operand1)
            (EBool operand0, EBool operand1) -> return $ EBool (operand0 && operand1)
            _ -> return EFail

eval (Gt value0 value1) = comparison (>) value0 value1
eval (Ge value0 value1) = comparison (>=) value0 value1
eval (Le value0 value1) = comparison (<=) value0 value1
eval (Eq value0 value1) = comparison (==) value0 value1
eval (Ne value0 value1) = comparison (/=) value0 value1
eval (Lt value0 value1) = comparison (<) value0 value1
eval (Plus value0 value1) = arith (+) value0 value1
eval (Times value0 value1) = arith (*) value0 value1
eval (Div value0 value1) = arith div value0 value1
eval (Minus value0 value1) = arith (-) value0 value1
eval (LeftShift value0 value1) = arith (shift) value0 value1
eval (RightShift value0 value1) = arith (shift . negate) value0 value1

eval (PeekByte expr) = do
        value <- eval expr
        case value of
            EInt addr -> do
                byte <- readMemory (fromIntegral addr)
                return (EInt $ fromIntegral byte)
            _ -> return EFail

eval (PeekWord expr) = do
        value <- eval expr
        case value of
            EInt addr -> do
                lo <- readMemory (fromIntegral addr)
                hi <- readMemory (fromIntegral addr+1)
                return (EInt $ fromIntegral $ Core.make16 lo hi)
            _ -> return EFail

eval (Not expr) = do
    value <- eval expr
    case value of
        EBool operand -> return $ EBool (not operand)
        EInt operand -> return $ EInt (-1-operand)
        _ -> return EFail

eval (EConst number) = return (EInt number)
eval (EConstString text) = return (EString text)

eval expr = do
    liftIO $ print expr
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
