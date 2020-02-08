module Ex1 where

{-- Ex 1
    
    Define an instance of the Functor class for the following type of binary trees that have data in their nodes:
    data Tree a = Leaf | Node (Tree a) a (Tree a)
 --}

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree a where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)