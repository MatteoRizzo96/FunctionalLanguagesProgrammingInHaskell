module Ex4 where

-- Computes the Fibonacci series as the sums 0:(1:(1):2) ...
fibs :: [Integer]
fibs = 0:1:[x + y | (x, y) <- zip fibs (tail fibs)]