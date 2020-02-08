import Data.List
import Data.Ord

{-
    TOWERS OF HANOI 

    The Towers of Hanoi is a classic puzzle with a solution that can be described recursively.
    Disks of different sizes are stacked on 3 pegs; the goal is to get from a starting configuration with
    all disks stacked on the first peg to an ending configuration with all disks stacked on the last peg.

    The only rules are:
    - you may only move one disk at a time, and
    - a larger disk may never be stacked on top of a smaller one.

    Example: as the first move all you can do is move the topmost,smallest disk onto a different peg,
    since only one disk may be moved at a time. From this point, you are not allowed to put a bigger disk on top of a smaller one.

    To move N discs (stacked in increasing size) from peg A to peg B using peg C as temporary storage,
    1. move N-1 discs from A to C using B as temporary storage
    2. move the top disc from A to B
    3. move N-1 discs from C to B using A as temporary storage.

    What if there are four pegs instead of three? That is, the goal is still to move a stack of discs
    from the first peg to the last peg, without ever placing a larger disc on top of a smaller one,
    but now there are two extra pegs that can be used as “temporary” storage instead of only one.
    Write a function similar to hanoi which solves this problem in as few moves as possible.
    It should be possible to do it in far fewer moves than with three pegs. 
    
    Example: with three pegs it takes 2^15 − 1 = 32767 moves to transfer 15 discs. With four pegs it can be done in 129.

    Given the number of discs and names for the 4 pegs, hanoi4 returns a list of moves to be performed 
    to move the stack of discs from the first peg to the second.
    
    Example: hanoi4 2 "a" "b" "c" "d" == [("a","c"), ("a","b"), ("c","b")]
-}

type Peg = String
type Move = (Peg, Peg)

-- Tower of Hanoi with 3 pegs
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 1 a b c = [(a, b)]
hanoi3 n a b c = hanoi3 (n - 1) a c b ++ [(a, b)] ++ hanoi3 (n - 1) c b a  

-- Tower of Hanoi with 4 pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b c d = [(a, b)]
hanoi4 n a b c d = shortest [hanoi4 (n - k) a d c b ++ hanoi3 k a b c ++ hanoi4 (n - k) d b a c | k <- [1..(n - 1)]]

-- Finds the shortest list in a list of lists
shortest :: [[a]] -> [a]
shortest [] = []
shortest ls = minimumBy (comparing length) ls

-- Pretty prints the solution
solveHanoi4 :: Integer -> IO ()
solveHanoi4 n = let sol = hanoi4 n "a" "b" "c" "d"
                in putStr ("\n Tower of Hanoi with 4 pegs and " ++ show n ++ " discs solved in " ++ show (length sol) ++ " moves: \n ---> " ++ show sol ++ "\n \n")