module Expr where
import Control.Applicative

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap _ Val         = Val
    fmap f (Var x)     = Var (f x)
    fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2) 

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    Val   <*> _     = Val
    Var f <*> e     = f <$> e
    Add f1 f2 <*> e = Add (f1 <*> e) (f2 <*> e)

instance Monad Expr where
    -- return :: a -> Expr a
    return = pure

    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Val   >>= _     = Val
    Var x >>= f     = f x
    Add e1 e2 >>= f = Add (e1 >>= f) (e2 >>= f)
    
