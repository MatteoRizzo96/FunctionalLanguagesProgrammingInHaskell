module ZipList where
import Control.Applicative

newtype MyZipList a = Z [a] deriving Show

app :: MyZipList a -> [a]
app (Z zs) = zs

instance Functor MyZipList where
    -- fmap :: (a -> b) -> MyZipList a -> MyZipList b
    fmap f (Z zs) = Z (f <$> zs) 

instance Applicative MyZipList where
    -- pure a -> MyZipList a
    pure z = Z (repeat z)

    -- (<*>) :: MyZipList (a -> b) -> MyZipList a -> MyZipList b
    Z fs <*> Z zs = Z (fs <*> zs)   -- Z fs <*> Z zs = Z [f z | (f, z) <- zip fs zs]

instance Monad MyZipList where
    -- return :: a -> MyZipList a
    return = pure

    -- (>>=) :: MyZipList a -> (a -> MyZipList b) -> MyZipList b
    Z zs >>= f = Z (concat [app (f z) | z <- zs])