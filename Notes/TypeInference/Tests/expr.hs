module Expr where

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

k (Var x)   g = Var (g x)
k (Val n)   g = Val n
k (Add x y) g = Add (k x g) (k y g)