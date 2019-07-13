module Debugger(runDebugger) where

import System.IO
import Asm
import AcornAtom
import Step
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import DebugCmd
import DebugState
import Disasm
import Emulation hiding (Command)
import Numeric
import System.Console.Haskeline
import Text.Parsec
import qualified Data.Map.Strict as Map

data DebugAction = Continue | KeepDebugging

comparison :: (Int -> Int -> Bool) -> Expr -> Expr -> MonadAcorn Value
comparison operator expr0 expr1 = do
    value0 <- eval expr0
    value1 <- eval expr1
    case (value0, value1) of
        (EInt operand0, EInt operand1) -> return $ EBool (operand0 `operator` operand1)
        _ -> return EFail

arith :: (Int -> Int -> Int) -> Expr -> Expr -> MonadAcorn Value
arith operator expr0 expr1 = do
    value0 <- eval expr0
    value1 <- eval expr1
    case (value0, value1) of
        (EInt operand0, EInt operand1) -> return $ EInt (operand0 `operator` operand1)
        _ -> return EFail

eval :: Expr -> MonadAcorn Value
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
            return (EInt $ fromIntegral $ Disasm.make16 lo hi)
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

disassemble :: Maybe Expr -> Maybe Expr -> MonadAcorn ()
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

-- Rewrite without case
execCommand :: Command -> MonadAcorn DebugAction
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
        Load f -> execLoad f
        Cont -> do
            liftIO $ putStrLn "Continuing..."
            return Continue
        Help -> execHelp
        Step -> (pc @-> pcStep) >> step >> return KeepDebugging
        Print es -> execPrint es
        Until cond repeatedCmd -> execUntil cond repeatedCmd
        Execute cmdExpr -> execute cmdExpr

execute :: Expr -> MonadAcorn DebugAction
execute cmdExpr = do
    cmdValue <- eval cmdExpr
    case cmdValue of
        EString cmdString -> do
            let cmd = parse parseCommands "" cmdString
            case cmd of
                Right cmd' -> execCommand cmd'
                Left e -> do
                    liftIO $ print e
                    return KeepDebugging
        _ -> return KeepDebugging

execRepeat :: Expr -> Command -> MonadAcorn DebugAction
execRepeat n repeatedCmd = do
    n' <- eval n
    case n' of
        EInt n'' -> do
            replicateM_ n'' (execCommand repeatedCmd)
        _ -> return ()
    return KeepDebugging

execPrint :: [Expr] -> MonadAcorn DebugAction
execPrint es = do
    forM_ es $ \e -> do
        val <- eval e
        case val of
            EInt w -> liftIO $ putStr ("0x" ++ showHex w "")
            _ -> liftIO $ putStr (show val)
    liftIO $ putStrLn ""
    return KeepDebugging

execLoad :: Expr -> MonadAcorn DebugAction
execLoad filexp = do
    f <- eval filexp
    case f of
        EString filename -> do
          handle <- liftIO $ openBinaryFile filename ReadMode
          contents <- liftIO $ hGetContents handle
          let romSize = length contents

          let addr = ord (contents!!16) + ord (contents!!17)*256
          liftIO $ putStrLn $ filename ++ ": ROM size = " ++ show romSize ++ " bytes."
          liftIO $ putStrLn $ "addr = " ++ showHex addr ""

          forM_ (zip [0..] contents) $ \(i, c) -> do -- Stupid me, I need to lose 1st 22 chars!
                liftIO $ print (showHex (i-0) "", showHex (ord c) "")
                writeMemory (i-0+fromIntegral addr) (fromIntegral (ord c))

        _ -> return ()

    return KeepDebugging

execHelp :: MonadAcorn DebugAction
execHelp = do
    liftIO $ do
        putStrLn ""
        putStrLn "Debugger Help"
        putStrLn "============="
        putStrLn "{<statement>;<statement>...} - block"
        putStrLn "Put multiple commands in a block, eg. r100{s;l} will step and list the current instruction 100 times."
        putStrLn ""
        putStrLn "c - continue"
        putStrLn "Return to playing game"
        putStrLn ""
        putStrLn "g - dump graphics state"
        putStrLn "Eg. r10{s;g} will step through 10 instructions dumping graphics state each step."
        putStrLn ""
        putStrLn "s - single instruction step"
        putStrLn "Eg. r100000s will step 100000 instructions"
        putStrLn ""
        putStrLn "r<expr><statement> - repeat statement"
        putStrLn "Eg. r(2*y){s;l} will step and list instructions a number of times given by double the Y register."
        putStrLn "(You can leave out the 'r' if the expression can be unambiguously read as an expression.)"
        putStrLn ""
        putStrLn "x<string> - execute command"
        putStrLn "Executes command in string. Eg. x\"p1\" will putStrLn 1."
        putStrLn ""
        putStrLn "l<expr>"
        putStrLn "l<expr>expr> - list disassembly"
        putStrLn "Disassemble from given expression with optional number of instructions."
        putStrLn "Eg. l(pc+2)10 lists 10 instructions starting at PC+2."
        putStrLn ""
        putStrLn "p<expr> - putStrLn"
        putStrLn "Print expression. Eg. p?0x9c7f putStrLns byte at address 0x9c7f"
        putStrLn ""
        putStrLn "u<expr><statement> - until"
        putStrLn "Perform statement until condition met."
        putStrLn "Eg. u(row==160){s};u(row>160){s;l} will step until row 160 of screen is reached and will then step, disassembling each instruction, until row 160 is finished."
        putStrLn "Eg. u(y>x){l;s} will step until Y register is larger than X."
        putStrLn ""
        putStrLn "<var>=<expr>"
        putStrLn "Set value of variable."
        putStrLn "Eg. U=1;V=2;pU+V will putStrLn 3."
        putStrLn ""
    return KeepDebugging

execUntil :: Expr -> Command -> MonadAcorn DebugAction
execUntil cond repeatedCmd = loop >> return KeepDebugging where
    loop = do
        c <- eval cond
        case c of
            EBool True -> return ()
            EBool False -> do
                void $ execCommand repeatedCmd
                loop
            _ -> liftIO $ putStrLn "Non-boolean condition"

runDebugger :: MonadAcorn ()
runDebugger = do
    let settings = defaultSettings { historyFile=Just ".stellarator" }
    mline <- liftIO $ runInputT settings $ getInputLine "> "
    case mline of
          Nothing -> do
              liftIO $ print "IO Error"
              runDebugger
          Just line -> do
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
