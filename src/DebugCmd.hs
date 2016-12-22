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
             | DumpGraphics | Step
             | Until Expr Command
             | Print [Expr]
             | List (Maybe Expr) (Maybe Expr)
             | Repeat Expr Command
             | Block [Command]
             | Let String Expr
              deriving (Show)

data Value = EFail | EInt Int | EBool Bool | EString String

instance Show Value where
    show EFail = "***error***"
    show (EInt n) = show n
    show (EBool b) = show b
    show (EString s) = s

data Expr = A | X | Y | PC | S
          | EQ | NE | CC | CS | PL | MI
          | Row | Col | Clock
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

parseCommand :: ParsecT String u Identity Command
parseCommand =
        Block <$> (braces lexer $ sepBy1 parseCommand (semi lexer))
    <|> (char 'c' >> many (char ' ') >> return Cont)
    <|> (char 'g' >> many (char ' ') >> return DumpGraphics)
    <|> (char 's' >> many (char ' ') >> return Step)
    <|> (do
        _ <- char 'r'
        _ <- many (char ' ')
        n <- parseExpr
        c <- parseCommand
        return $ Repeat n c)
    <|> (do
        _ <- char 'l'
        _ <- many (char ' ')
        addr <- optionMaybe parseExpr
        n <- optionMaybe parseExpr
        return (List addr n))
    <|> (do
            _ <- char 'p'
            _ <- many (char ' ')
            exprs <- sepBy1 parseExpr (comma lexer)
            return $ Print exprs)
    <|> (do
            _ <- char 'u'
            _ <- many (char ' ')
            expr <- parseExpr
            _ <- many (char ' ')
            cmd <- parseCommand
            return $ Until expr cmd)
    <|> (do
            s <- identifier lexer
            _ <- many (char ' ')
            _ <- char '='
            _ <- many (char ' ')
            e <- parseExpr
            return $ Let s e)

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellStyle

parseExpr :: ParsecT String u Identity Expr
parseExpr    = buildExpressionParser table term
        <?> "expression"

term :: ParsecT String u Identity Expr
term    =  parens lexer parseExpr 
        <|> try (symbol lexer "t" >> return Clock)
        <|> try (symbol lexer "col" >> return Col)
        <|> try (symbol lexer "row" >> return Row)
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
binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc
prefix :: String -> (a -> a) -> Operator String u Data.Functor.Identity.Identity a
prefix  name fun       = Prefix (do{ reservedOp lexer name; return fun })
postfix :: String -> (a -> a) -> Operator String u Data.Functor.Identity.Identity a
postfix name fun       = Postfix (do{ reservedOp lexer name; return fun })
