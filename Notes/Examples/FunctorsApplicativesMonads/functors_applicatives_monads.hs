{-
    A functor is a typeclass, which is similar to an interface.
    A functor implements "fmap", which is a function that takes as inputs:
    - a function (e.g. (+3))
    - a wrapped value (e.g. Just 3)
    And returns the wrapped result of the application of the function on the
    unwrapped input wrapped value.

    class Functor where
        fmap :: (a -> b) -> f a -> f b
-}

-- data Maybe a = Just a | Nothing

data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2

-- Haskell define functor for Maybe, Either, Lists

data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap func (Tip a) = Tip (func a)
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right)
    -- Or, equivalently:
    -- fmap func (Branch left right) = Branch (f <$> left) (f <$> right)

{-
    An Applicative is a typeclass, that is sort of an interface. 
    Applicatives allow to wrap functions (e.g. in lists) and to apply them on wrapped values.

    class functor f => Applicative f where
        pure :: a -> f a
        <*> :: f (a -> b) -> f a -> f b
-}

instance Applicative Maybe2 where
    pure = Just2
    Just2 f <*> (Just2 j) = Just2 (f j)
    -- Or, equivalently:
    -- Just2 f <*> j = fmap f j (i.e. fmap (+3) (Just2 j))
    Nothing2 <*> j = Nothing2
    
    
instance Applicative Tree where
    pure = Tip
    Tip f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)