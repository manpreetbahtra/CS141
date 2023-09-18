module Parsing where

{- Your code goes here! -}
{- The lab sheet contains all the instructions :) -}
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Control.Applicative.Combinators
import Data.Void
import Data.Char


type Parser = Parsec Void String

-- type Calculator = Parsec Void Integer|Add Integer Integer | Subtract Integer Integer| Multiply Integer Integer | Parens Integer| Number Integer
data Expr
    = Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Parens Expr
    | Number Int

parseNumber :: Parser Expr
parseNumber = do
    digits <- many isCharDigit
    return $ Number $ read digits

isCharDigit :: Parser Char
isCharDigit = satisfy isDigit
--satisfy function from Text.Megaparsec to match a single character that satisfies the isDigit predicate


parseExpr :: Parser Expr
parseExpr = choice [parsePlus, parseMinus, parseMultiply, parseParens, parseNumber]

parsePlus :: Parser Expr
parsePlus = do
    _<- char '+'
    expr1 <- parseExpr 
    expr2 <- parseExpr 
    pure $ Add expr1 expr2 
--cant do expr1 + expr2 - you need to make it Expr type as above

parseMinus :: Parser Expr
parseMinus = do
    _ <- char '-'
    expr1 <- parseExpr
    expr2 <- parseExpr
    pure $ Subtract expr1 expr2

parseMultiply :: Parser Expr
parseMultiply = do
    _ <- char '*'
    expr1 <- parseExpr
    expr2 <- parseExpr
    pure $ Multiply expr1 expr2

parseParens :: Parser Expr
parseParens = do
    _ <- char '('
    expr1 <- parseExpr
    _ <- char ')'
    pure $ Parens expr1 

-- parseNumber :: Parser Int
-- parseNumber = do
--     expr1 <- parseExpr
--     pure expr1 

eval :: Expr -> Int 
eval x = case x of
    Add a b -> eval a + eval b 
    Subtract a b -> eval a - eval b 
    Multiply a b  -> eval a * eval b 
    Parens a -> eval a 
    Number a -> a


parseOp :: Parser Expr
parseOp = do
    let
        parsePlus' = do 
            _ <- char '+'
            pure Add
        parseMinus' = do
            _ <- char '-'
            pure Subtract
        parseMultiply' = do
            _ <- char '*' 
            pure Multiply
    op <- parsePlus' <|> parseMinus' <|> parseMultiply' 
    _ <- char ' '
    x <- parseExpr
    _ <- char ' '
    y <- parseExpr
    return $ op x y 
