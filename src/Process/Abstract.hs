{-# LANGUAGE CPP #-}
module Process.Abstract
  ( parseFromString
  , execute
  ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (*>), (<*))

data Expr =
    Value Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Multi Expr Expr
  | Divide Expr Expr
  deriving Show

eval a b = foldl (\x f -> f x) <$> a <*> b

expr :: Parser Expr
expr = eval term $ many $
        (char '+' *> term >>= \x -> return $ Plus $ x)
    <|> (char '-' *> term >>= \x -> return $ Minus $ x)

term :: Parser Expr
term = eval factor $ many $
        (char '*' *> factor >>= \x -> return $ Multi $ x)
    <|> (char '/' *> factor >>= \x -> return $ Divide $ x)

factor :: Parser Expr
factor = spaces *> (char '(' *> expr <* char ')' <|> number) <* spaces

number :: Parser Expr
number = do
    x <- many1 digit
    return $ Value $ (read x :: Int)

parseFromString :: String -> Either ParseError Expr
parseFromString = parse expr ""

execute :: String -> Either ParseError Int
execute x = case parse expr "" x of
    Right n -> Right $ calc $ n

calc :: Expr -> Int
calc (Value x) = x
calc (Plus x y) = calc(y) + calc(x)
calc (Minus x y) = calc(y) - calc(x)
calc (Multi x y) = calc(y) * calc(x)
calc (Divide x y) = calc(y) `div` calc(x)
