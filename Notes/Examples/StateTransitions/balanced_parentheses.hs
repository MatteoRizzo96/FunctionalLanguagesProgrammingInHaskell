module BalancedParentheses where
import Control.Applicative

type Stack = String
newtype PDA a = P (Stack -> [(a, Stack)])

app :: PDA a -> Stack -> [(a, Stack)]
app (P p) = p

instance Functor PDA where
    -- fmap :: (a -> b) -> PDA a -> PDA b
    -- fmap f (P px) = P (\s -> case px s of []        -> []
    --                                       [(x, s')] -> [(f x, s')])
    fmap f pdax = do x <- pdax
                     return (f x)

instance Applicative PDA where
    -- pure :: a -> PDA a
    pure x = P (\s -> [(x, s)])

    -- (<*>) :: PDA (a -> b) -> PDA a -> PDA b
    -- (P pf) <*> pdax = P (\s -> case pf s of []        -> []
    --                                         [(f, s')] -> let (P x') = f <$> pdax in x' s')
    pdaf <*> pdax = do f <- pdaf
                       x <- pdax
                       return (f x) 

instance Monad PDA where
    -- return :: a -> PDA a
    return = pure

    -- (>>=) :: PDA a -> (a -> PDA b) -> PDA b
    (P px) >>= f = P (\s -> case px s of []        -> []
                                         [(x, s')] -> app (f x) s')

pop :: PDA Char
pop = P (\s -> case s of []     -> []
                         (x:xs) -> [(x, xs)])

push :: PDA ()
push = P (\s -> [((), '(':s)])

balance :: String -> PDA Bool
balance []       = P (\s -> case s of [] -> [(True, s)]
                                      _  -> [(False, s)])
balance ('(':xs) = do {push; balance xs} 
balance (')':xs) = do {pop; balance xs} 
balance (_:xs)   = balance xs

{- Check that in a given string the parentheses are balanced. This function calls balance and
   prints whether the input is balanced or not. In case the input is not balanced, it should
   explain what error was found.

   Examples: 1) "(1 (2 (3) 4) 5)" -> "Balanced!"
             2) "(1 (2 (3  4) 5)" -> "Error: Too many opened parentheses"
             3) " 1 (2 (3) 4) 5)" -> "Error: Too many closed parentheses"
-}
execute :: String -> IO ()
execute xs = case app (balance xs) [] of 
             []             -> print (xs ++ " -> Error: Too many closed parentheses")
             [(False, s)]   -> print (xs ++ " -> Error: Too many opened parentheses")
             [(True, [])]   -> print (xs ++ " -> Balanced!")
             _              -> print (xs ++ " -> Fatal error: > This should not have happened! <")
