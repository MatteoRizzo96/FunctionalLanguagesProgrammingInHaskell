module Ex5 where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- repeat :: a -> [a]
-- repeat x = xs where xs = x:xs

repeatTree :: a -> Tree a
repeatTree x = t where t = Node t x t

-- take :: Int -> [a] -> [a]
-- take 0 _	= []
-- take _ []	= []
-- take n (x:xs) = x : take (n - 1) xs

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _	    = Leaf
takeTree _ Leaf	= Leaf
takeTree n (Node l x r) = Node (takeTree (n - 1) l) x (takeTree (n - 1) r)

-- replicate :: Int -> a -> [a]
-- replicate n = take n . repeat oa 

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree 
