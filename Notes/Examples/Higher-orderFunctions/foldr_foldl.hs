module FoldrFoldl where

-- Redefinition of map
mapR :: (a -> b) -> [a] -> [b]
mapR f = foldr (\x xs -> f x : xs) []

mapL :: (a -> b) -> [a] -> [b]
mapL f = foldl (\xs x -> xs ++ [f x]) []

-- Redefinition of filter
filterR :: (a -> Bool) -> [a] -> [a]
filterR p = foldr (\x xs -> if p x then x : xs else xs) []

filterL :: (a -> Bool) -> [a] -> [a]
filterL p = foldl (\xs x -> if p x then xs ++ [x] else xs) []

-- Redefinition of reverse
reverseR :: [a] -> [a]
reverseR = foldr (\y ys -> ys ++ [y]) []

reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) [] -- reverseL = foldl (\ys y -> y:ys) []

-- Redefinition of append
appendR :: [a] -> [a] -> [a]
appendR xs ys = foldr (:) ys xs

appendL :: [a] -> [a] -> [a]
appendL = foldl (\ ys y -> ys ++ [y])

-- Redefinition of last
lastL :: [a] -> a
lastL (y:ys) = foldl (\xs x -> x) y ys

lastR :: [a] -> a
lastR (y:ys) = foldr const y (reverse ys)

-- Redefinition of length
lengthR :: [a] -> Int
lengthR = foldr (\x v -> v + 1) 0 

lengthL :: [a] -> Int
lengthL = foldl (\v x -> v + 1) 0 

-- Decimal to integer conversion
dec2int :: [Int] -> Int
dec2int = foldl (\v x -> v * 10 + x) 0