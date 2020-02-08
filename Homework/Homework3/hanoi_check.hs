module Hanoi_check where
import Data.List
import Data.Ord
import Control.Applicative

-- HANOI TOWER WITH 4 PEGS

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

-- HANOI CHECKER

{-
    Given a list of moves for the Hanoi Towers with 4 pegs and an initial configuration, check that all moves are legitimate.
    The type of the result of check is ((Report, [Move]), (Int, Config)), which consists of 2 pairs:

        a) (Report, [Move]) indicates whether the sequence of moves is Ok/Bad and:
            * If the first component is Bad, the second component contains a list with the wrong move
              that was encountered. The list contains just this move
            * If the first component is Ok, then the second component is simply the empty list []

        b) (Int, Config):
            * Int: indicates the number of moves that are executed till the computation stops,
              either because a wrong move was found or because all moves have been successfully checked.
              Initially this number is 0.
            * Config: indicates the configuration reached when the computation stops:
                - In case of success, it will be the configuration reached after the last move is executed
                - In case of a wrong move, it is the (correct) configuration just before the wrong move is considered
                  (the wrong move in not executed)

    Examples: (1) check [("a", "b"), ("b", "c"), ("a", "b")] (0, [[1,2,3,4],[],[],[]])
                  --> (Ok, []), (3, [[3,4],[2],[1],[]])
              (2) check [("a", "b"), ("b", "c"), ("a", "c")] (0, [[1,2,3,4],[],[],[]])
                  --> (Bad, [("a", "c")]), (2, [[2,3,4],[],[1],[]]) i.e. bad move, larger disc on top of smaller
-}

type Config = [[Int]]
data Report = Bad | Ok deriving Show
type State   = (Int, Config)
newtype ST a = S (State -> (a, State))
pegs = ["a", "b", "c", "d"]

-- ............. Common utility functions ...............

idx :: String -> Int
idx c = snd (head (filter (\x -> fst x == c) (zip pegs [0 .. length pegs - 1])))

checkMove :: Config -> Move -> Bool
checkMove c (xs, ys) = pxs /= []                          &&  -- 1) It does not ask to move a disc from an empty peg
                       (null pys || head pxs < head pys)  &&  -- 2) It never puts a disc on top of a smaller disc
                       elem xs pegs && elem ys pegs           -- 3) It only uses pegs "a", "b", "c" and "d" 
                       where pxs = c !! idx xs 
                             pys = c !! idx ys 

-- .................... V1 (No Monads) ................... 

-- updateConfig :: Config -> Move -> Config
-- updateConfig c (xs, ys) = updatePeg <$> pegs where updatePeg p | p == xs   = tail (c !! idx xs)
--                                                                | p == ys   = head (c !! idx xs) : (c !! idx ys) 
--                                                                | otherwise = c !! idx p  

-- check :: [Move] -> (Int, Config) -> ((Report, [Move]), (Int, Config))
-- check []     (n, c) = ((Ok, []), (n, c))
-- check (m:ms) (n, c) = if checkMove c m then check ms (n + 1, updateConfig c m) else ((Bad, [m]), (n, c))

-- ...................... V2 (Maybe) ..................... 

-- updateConfig :: Config -> Move -> Maybe Config
-- updateConfig c m@(xs, ys) = if checkMove c m then Just (updatePeg <$> pegs) else Nothing 
--                             where updatePeg p | p == xs   = tail (c !! idx xs)
--                                               | p == ys   = head (c !! idx xs) : (c !! idx ys) 
--                                               | otherwise = c !! idx p  

-- check :: [Move] -> (Int, Config) -> ((Report, [Move]), (Int, Config))
-- check []     (n, c) = ((Ok, []), (n, c))
-- check (m:ms) (n, c) = case updateConfig c m of Nothing -> ((Bad, [m]), (n, c))
--                                                Just c' -> check ms (n + 1, c')

-- ............... V3 (State Transitions) ................

app :: ST a -> State -> (a, State)
app (S x) = x 

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f stx = do x <- stx
                    return (f x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)  

instance Monad ST where
    -- return :: a -> ST a
    return = pure

    -- (>>=) :: ST a -> a -> ST b -> ST b
    stx >>= f = S (\s -> let (x, s') = app stx s in app (f x) s')

isLegal :: Move -> ST Bool
isLegal m = S (\s@(n, c) -> (checkMove c m, s))      

perform :: Move -> Config -> Config
perform (xs, ys) c = updatePeg <$> pegs 
                     where updatePeg p | p == xs   = tail (c !! idx xs)
                                       | p == ys   = head (c !! idx xs) : (c !! idx ys) 
                                       | otherwise = c !! idx p  

move :: Move -> ST Report
move m = do l <- isLegal m
            if l then S (\(n, c) -> (Ok, (n + 1, perform m c))) else return Bad  

checkMoves :: [Move] -> ST (Report, [Move])
checkMoves []     = return (Ok, [])
checkMoves (m:ms) = do q <- move m
                       case q of Bad -> return (Bad, [m])
                                 Ok  -> checkMoves ms 

check :: [Move] -> State -> ((Report, [Move]), State)
check ms = app (checkMoves ms)