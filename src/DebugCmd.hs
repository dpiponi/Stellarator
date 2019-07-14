{-# LANGUAGE FlexibleContexts #-}

module DebugCmd where

import Prelude hiding (EQ)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (haskellStyle)
import Data.Word
import Data.Functor.Identity
import Numeric

data Command = Cont
             | Help
             | Step
             | Until Expr Command
             | Print [Expr]
             | List (Maybe Expr) (Maybe Expr)
             | Repeat Expr Command
             | Block [Command]
             | Let String Expr
             | Execute Expr
             | Load Expr
             | SaveState Expr
             | LoadState Expr
              deriving (Show)

data Value = EFail | EInt Int | EBool Bool | EString String

instance Show Value where
    show EFail = "***error***"
    show (EInt n) = show n
    show (EBool b) = show b
    show (EString s) = s

data Expr = A | X | Y | PC | S
          | EQ | NE | CC | CS | PL | MI
          | Clock
          | Var String
          | Gt Expr Expr | Lt Expr Expr
          | Ge Expr Expr | Le Expr Expr
          | Ne Expr Expr | Eq Expr Expr
          | EConst Int
          | EConstString String
          | Neg Expr
          | Or Expr Expr | And Expr Expr
          | Not Expr
          | Plus Expr Expr | Times Expr Expr
          | Minus Expr Expr | Div Expr Expr
          | PeekByte Expr | PeekWord Expr
          | RightShift Expr Expr | LeftShift Expr Expr
            deriving (Show)

fromHex :: (Num a, Eq a) => String -> a
fromHex = fst . head . readHex

hexWord :: Stream s m Char => ParsecT s u m Word16
hexWord = fromHex <$> many1 hexDigit

parseCommands:: ParsecT String u Identity Command
parseCommands = Block <$> semiSep1 lexer parseCommand

parseCommand :: ParsecT String u Identity Command
parseCommand = Block <$> (braces lexer $ semiSep1 lexer parseCommand)
           <|> (char 'c' >> whiteSpace lexer >> return Cont)
           <|> (char 's' >> whiteSpace lexer >> return Step)
           <|> (char 'h' >> whiteSpace lexer >> return Help)
           <|> Repeat <$> (char 'r' >> whiteSpace lexer >> parseExpr) <*> parseCommand
           <|> Execute <$> (char 'x' >> whiteSpace lexer >> parseExpr)
           <|> List <$> (char 'l' >> whiteSpace lexer >> optionMaybe parseExpr) <*> optionMaybe parseExpr
           <|> Print <$> (char 'p' >> whiteSpace lexer >> sepBy1 parseExpr (comma lexer))
           <|> Until <$> (char 'u' >> whiteSpace lexer >> parseExpr) <*> parseCommand
           <|> Let <$> (identifier lexer <* char '=' <* whiteSpace lexer) <*> parseExpr
           <|> Load <$> (char '*' >> whiteSpace lexer >> parseExpr)
           <|> Repeat <$> parseExpr <*> parseCommand
           <|> LoadState <$> (char '<' >> whiteSpace lexer >> parseExpr)
           <|> SaveState <$> (char '>' >> whiteSpace lexer >> parseExpr)

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellStyle

parseExpr :: ParsecT String u Identity Expr
parseExpr = buildExpressionParser table term
        <?> "expression"

term :: ParsecT String u Identity Expr
term  = parens lexer parseExpr 
    <|> try (symbol lexer "t" >> return Clock)
    <|> try (symbol lexer "eq" >> return EQ)
    <|> try (symbol lexer "ne" >> return NE)
    <|> try (symbol lexer "cc" >> return CC)
    <|> try (symbol lexer "cs" >> return CS)
    <|> try (symbol lexer "pl" >> return PL)
    <|> try (symbol lexer "pc" >> return PC)
    <|> try (symbol lexer "mi" >> return MI)
    <|> (symbol lexer "a" >> return A)
    <|> (symbol lexer "x" >> return X)
    <|> (symbol lexer "y" >> return Y)
    <|> (symbol lexer "s" >> return S)
    <|> do { n <- natural lexer; return (EConst (fromIntegral n)) }
    <|> do { s <- identifier lexer ; return (Var s) }
    <|> do { s <- stringLiteral lexer ; return (EConstString s) }
    <?> "simple expression"

table :: [[Operator String u Identity Expr]]
table   = [ [prefix "-" Neg, prefix "+" id,
             prefix "!" PeekWord, prefix "?" PeekByte],

            [prefix "~" Not],

            [binary "*" Times AssocLeft,
             binary "/" Div AssocLeft ],

            [binary "-" Minus AssocLeft,
             binary "+" Plus AssocLeft ],

            [binary "<<" LeftShift AssocLeft,
             binary ">>" RightShift AssocLeft ],

            [binary "<" Lt AssocLeft,
             binary ">" Gt AssocLeft,
             binary ">=" Gt AssocLeft,
             binary "<=" Le AssocLeft,
             binary "!=" Ne AssocLeft,
             binary "==" Eq AssocLeft],

            [binary "&" And AssocLeft], 

            [binary "|" Or AssocLeft]
          ]
        
binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary  name fun assoc = Infix   (do{ reservedOp lexer name; return fun }) assoc
prefix :: String -> (a -> a) -> Operator String u Data.Functor.Identity.Identity a
prefix  name fun       = Prefix  (do{ reservedOp lexer name; return fun })
postfix :: String -> (a -> a) -> Operator String u Data.Functor.Identity.Identity a
postfix name fun       = Postfix (do{ reservedOp lexer name; return fun })
