module RecursiveTree where

data Tree a = Leaf a | Node a (Tree a) (Tree a)

k (Leaf x)     g = x							-- (1)
k (Node x y z) g = Node (g x) (k y g) (k z g)	-- (2)