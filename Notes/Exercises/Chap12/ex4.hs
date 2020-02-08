module Ex4 where

{- Ex 4

   Define functor and applicative for the type ZipList 
-}

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z zs) = Z (fmap g zs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    Z fz <*> Z xz = Z [f x | (f, x) <- zip fz xz] 