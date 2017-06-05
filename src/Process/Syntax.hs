{-# LANGUAGE CPP #-}
module Process.Syntax
  ( Expr
  , Func
  ) where

data Func =
    Let Expr [Func]

data Expr =
    Unit -- ()
  | True -- true
  | False -- false
  | Int Int -- integer
  | Float Float -- float
  | Var String -- identifier
  | Not Expr -- not exp
  | Neg Expr -- - exp
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

