{-# LANGUAGE CPP #-}
module Process.Example03
  ( parseFromString
  ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>), (*>), (<*))

data Func =
    Let Expr Expr
  | LetIn Expr Expr [Func]
  deriving Show

data Expr =
    Unit -- ()
  | Bool Bool -- true | false
  | Int Int -- integer
  | Var String -- identifier
  | Add Expr Expr -- exp + exp
  | Sub Expr Expr -- exp - exp
  | Mul Expr Expr -- exp * exp
  | Div Expr Expr -- exp / exp
  | Eq Expr Expr -- exp == exp
  | Lt Expr Expr -- exp < exp
  | Gt Expr Expr -- exp > exp
  | Lte Expr Expr -- exp <= exp
  | Gte Expr Expr -- exp >= exp
  | If Expr Expr Expr --- if exp then exp else exp
  | App Expr [Expr] -- exp exp exp
  deriving Show

func :: Parser Func
func = try defin <|> def

def :: Parser Func
def = do
    string "let"
    spaces
    x <- expr
    spaces
    char '='
    spaces
    y <- expr
    return $ Let x y

defin :: Parser Func
defin = do
    string "let"
    spaces
    x <- expr
    spaces
    char '='
    spaces
    y <- expr
    spaces
    string "in"
    spaces
    xs <- many1 func
    spaces
    string "end"
    return $ LetIn x y xs

eval a b = foldl (\x f -> f x) <$> a <*> b

expr :: Parser Expr
expr = eval term $ many $
        (char '+' *> term >>= \x -> return $ Add $ x)
    <|> (char '-' *> term >>= \x -> return $ Sub $ x)

term :: Parser Expr
term = eval factor $ many $
        (char '*' *> factor >>= \x -> return $ Mul $ x)
    <|> (char '/' *> factor >>= \x -> return $ Div $ x)

factor :: Parser Expr
factor = spaces *> (char '(' *> expr <* char ')' <|> int <|> var) <* spaces

int :: Parser Expr
int = do
    x <- many1 digit
    return $ Int $ (read x :: Int)

var :: Parser Expr
var = do
    x <- many1 letter
    return $ Var $ x

unit :: Parser Expr
unit = do
    string "()"
    return $ Unit

bool :: Parser Expr
bool =
      (string "true" >>= \_ -> return $ Bool $ True)
  <|> (string "false" >>= \_ -> return $ Bool $ False)

parseFromString :: String -> Either ParseError Func
parseFromString = parse func ""

