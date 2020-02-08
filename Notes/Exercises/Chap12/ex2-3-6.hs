module Ex2_3_6 where

{-- Ex 2

    Complete the following instance declaration to make the partially-applied function type (a ->) into a functor:
    instance Functor ((->) a) where
    
    Hint: first write down the type of fmap, and then think if you already know a library function that has this type

    Ex 3

    Define an instance of the Applicative class for the type (a ->). If you are familiar with combinatory logic, 
    you might recognize pure and <*> for this type as being the well-know K and S combinators 

    Ex 6

    Define an instance of the Monad class for the type (a ->).
    Remind that:
        return :: a   -> m a
        (>>=)  :: m a -> (a -> m b) -> m b
        (>>)   :: m a -> m b        -> m b
--}

instance Functor ((->) r) where
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap = (.)

instance Applicative ((->) r) where
    -- pure :: a -> (r -> a)
    pure = const 

    -- <*> :: ((r -> a) -> b) -> (r -> a) -> (r -> b)
    f <*> g = \x -> f x (g x) 

instance Monad ((->) r) where
    -- return :: a -> (r -> a)  
    return = pure

    -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
    f >>= g = \x -> g (f x) x
