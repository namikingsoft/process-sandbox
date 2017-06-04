{-# LANGUAGE CPP #-}
module Process.Arithmetic
  ( execute
  ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (*>), (<*))

eval a b = foldl (\x f -> f x) <$> a <*> b

expr :: Parser Int
expr = eval term $ many $
        (char '+' *> term >>= \x -> return (+ x))
    <|> (char '-' *> term >>= \x -> return (subtract x))

term :: Parser Int
term = eval factor $ many $
        (char '*' *> factor >>= \x -> return (* x))
    <|> (char '/' *> factor >>= \x -> return (`div` x))

factor :: Parser Int
factor = spaces *> (char '(' *> expr <* char ')' <|> number) <* spaces

number :: Parser Int
number = do
    x <- many1 digit
    return (read x :: Int)

execute :: String -> Either ParseError Int
execute = parse expr ""
