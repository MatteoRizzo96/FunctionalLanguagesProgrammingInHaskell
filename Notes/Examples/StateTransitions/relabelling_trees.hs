module RelabellingTrees where
import Control.Applicative

-- State transitions

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    -- fmap f stx = S (\s -> let (x, s') = app stx s in (f x, s')) 
    fmap f stx = do x <- stx
                    return (f x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    -- stf <*> stx = S (\s -> let (f, s')  = app stf s
    --                            (x, s'') = app stx s' in (f x, s''))
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)

instance Monad ST where
    -- return :: a -> ST a
    return = pure

    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

-- Tree type with data on leaves
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- State transformer
fresh :: ST Int
fresh = S (\n -> (n, n + 1))

-- Applicative style relabelling
relabelA :: Tree a -> ST (Tree Int)
relabelA (Leaf _)   = Leaf <$> fresh
relabelA (Node l r) = Node <$> relabelA l <*> relabelA r

-- Monadic style relabelling
relabelM :: Tree a -> ST (Tree Int)
relabelM (Leaf _)   = do n <- fresh
                         return (Leaf n)
relabelM (Node l r) = do l' <- relabelM l
                         r' <- relabelM r
                         return (Node l' r')

