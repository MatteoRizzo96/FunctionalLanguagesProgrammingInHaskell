module Ex7 where
import Control.Applicative

{-- Ex 7

    Given the following type of expressions:
    
    data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show
    
    that contain variables of some type a, show how to make this type into instances of the Functor, Applicative and Monad classes.
    With the aid of an example, explain what the >>= operator for this type does
--}

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Var x) = Var (g x)
    fmap _ (Val x) = Val x
    fmap g (Add e1 e2) = Add (fmap g e1) (fmap g e2)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    (Val n) <*> _ = Val n
    (Var x) <*> e = x <$> e 
    (Add e1 e2) <*> y = Add (e1 <*> y) (e1 <*> y)

instance Monad Expr where
    -- return :: a -> Expr a
    return = pure

    -- >>= :: Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= g = g x
    (Val n) >>= _ = Val n
    (Add e1 e2) >>= g = Add (e1 >>= g) (e2 >>= g)
