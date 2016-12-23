module Debugger(runDebugger) where

import Asm
import Atari2600
import Control.Monad
import Control.Monad.State.Strict
import Core
import Data.Bits
import DebugCmd
import DebugState
import Disasm
import Emulation()
import VideoOps
import System.Console.Haskeline
import Text.Parsec
import qualified Data.Map.Strict as Map

data DebugAction = Continue | KeepDebugging

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
eval A = EInt <$> fromIntegral <$> getA
eval X = EInt <$> fromIntegral <$> getX
eval Y = EInt <$> fromIntegral <$> getY
eval PC = EInt <$> fromIntegral <$> getPC
eval DebugCmd.S = EInt <$> fromIntegral <$> getS
eval DebugCmd.EQ = EBool <$> getZ
eval NE = EBool <$> not <$> getZ
eval CS = EBool <$> getC
eval CC = EBool <$> not <$> getC
eval MI = EBool <$> getN
eval PL = EBool <$> not <$> getN
eval DebugCmd.Clock = EInt <$> fromIntegral <$> useClock id
eval Row = EInt <$> fromIntegral <$> load vpos
eval Col = EInt <$> fromIntegral <$> load hpos

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
        EInt  operand -> return $ EInt  (-1-operand)
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

execCommand :: Command -> MonadAtari DebugAction
execCommand cmd = 
    case cmd of
        Let var e -> do
            e' <- eval e
            modifyStellaDebug variables $ Map.insert var e'
            _ <- useStellaDebug variables -- Uh? XXX
            return KeepDebugging
        Block cmds -> do
            loop KeepDebugging cmds where
                loop Continue      _        = return Continue
                loop KeepDebugging []       = return KeepDebugging
                loop KeepDebugging (c : cs) = do
                    action <- execCommand c
                    loop action cs
        DebugCmd.List addr n -> do
            disassemble addr n
            return KeepDebugging
        Repeat n repeatedCmd -> execRepeat n repeatedCmd
        Cont -> do
            liftIO $ putStrLn "Continuing..."
            return Continue
        DumpGraphics -> dumpStella >> return KeepDebugging
        Step -> step >> return KeepDebugging
        Print es -> execPrint es
        Until cond repeatedCmd -> execUntil cond repeatedCmd

execRepeat :: Expr -> Command -> MonadAtari DebugAction
execRepeat n repeatedCmd = do
    n' <- eval n
    case n' of
        EInt n'' -> do
            replicateM_ n'' (execCommand repeatedCmd)
        _ -> return ()
    return KeepDebugging

execPrint :: [Expr] -> MonadAtari DebugAction
execPrint es = do
    forM_ es $ \e -> do
        val <- eval e
        liftIO $ putStr (show val)
    liftIO $ putStrLn ""
    return KeepDebugging

execUntil :: Expr -> Command -> MonadAtari DebugAction
execUntil cond repeatedCmd = loop >> return KeepDebugging where
    loop = do
        c <- eval cond
        case c of
            EBool True -> return ()
            EBool False -> do
                void $ execCommand repeatedCmd
                loop
            _ -> liftIO $ putStrLn "Non-boolean condition"

runDebugger :: MonadAtari ()
runDebugger = do
    Just line <- liftIO $ runInputT (defaultSettings { historyFile=Just ".stellarator" }) $ getInputLine "> "
    let cmd = parse parseCommands "" line
    case cmd of
        Right cmd' -> do
            q <- execCommand cmd'
            case q of
                KeepDebugging -> runDebugger
                Continue      -> return ()
        Left e -> do
            liftIO $ print e
            runDebugger
