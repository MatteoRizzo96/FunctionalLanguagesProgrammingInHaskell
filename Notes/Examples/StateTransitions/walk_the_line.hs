module WalkTheLine where
import Control.Applicative
import Data.Maybe

{-
    WALK THE LINE

    Pierre has decided to take a break from his job at the fish farm and try tightrope walking.
    He's not that bad at it, but he does have one problem: birds keep landing on his balancing pole!
    They come and they take a short rest, chat with their avian friends and then take off in search of breadcrumbs.
    This would not bother him so much if the number of birds on the left side of the pole was always equal
    to the number of birds on the right side. But sometimes, all the birds decide that they like one side better
    and so they throw him off balance, which results in an embarrassing tumble for Pierre (he's using a safety net).

    Let's say that he keeps his balance if the number of birds on the left side of the pole and on the right side 
    of the pole is within three. So if there's one bird on the right side and four birds on the left side, he's okay.
    But if a fifth bird lands on the left side, then he loses his balance and takes a dive.

    We're going to simulate birds landing on and flying away from the pole and see if Pierre is still at it after 
    a certain number of arrivals and departures of birds. For instance, we want to see what happens to Pierre 
    if first one bird arrives on the left side, then four birds occupy the right side and then the bird 
    that was on the left side decides to fly away.

    We may model the problem using the following functions:

        landLeft :: Birds -> Pole -> Pole  
        landLeft n (left,right) = (left + n,right)  
        
        landRight :: Birds -> Pole -> Pole  
        landRight n (left,right) = (left,right + n) 

    * Example of execution: (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2 outputs (3,1)

    However, this way we have lost the possibility to fail. We need to use monads to regain the ability to
    repeatedly land birds on the pole while possibly failing. In particular, we can exploit the Maybe type,
    that is indeed a monadic type.

        landLeft :: Birds -> Pole -> Maybe Pole  
        landLeft n (left,right)  
            | abs ((left + n) - right) < 4 = Just (left + n, right)  
            | otherwise                    = Nothing  
        
        landRight :: Birds -> Pole -> Maybe Pole  
        landRight n (left,right)  
            | abs (left - (right + n)) < 4 = Just (left, right + n)  
            | otherwise                    = Nothing

        banana :: Pole -> Maybe Pole  
        banana _ = Nothing 

    * Example of execution: return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2, outputs Just (2,4)  
-}

type Birds = Int  
type Pole = (Birds,Birds)  
newtype Line a = L (Pole -> (Maybe a, Pole))

app :: Line a -> Pole -> (Maybe a, Pole)
app (L l) = l

instance Functor Line where
    -- fmap :: (a -> b) -> Line a -> Line b
    -- fmap f lx = L (\p -> let (mx, p') = app lx p in case mx of Nothing -> (Nothing, p')
    --                                                            Just x  -> (Just (f x), p'))
    fmap f lx = do x <- lx
                   return (f x)

instance Applicative Line where
    -- pure :: a -> Line a
    pure x = L (\p -> (Just x, p))

    -- (<*>) :: Line (a -> b) -> Line a -> Line b
    -- lf <*> lx = L (\p -> let (mf, p') = app lf p in case mf of Nothing -> (Nothing, p')
    --                                                            Just f  -> app (f <$> lx) p')
    lf <*> lx = do f <- lf
                   x <- lx
                   return (f x)

instance Monad Line where
    -- return :: a -> Line a
    return = pure

    -- (>>=) :: Line a -> (a -> Line b) -> Line b
    lx >>= f = L (\p -> let (mx, p') = app lx p in case mx of Nothing -> (Nothing, p') 
                                                              Just x  -> app (f x) p')

landLeft :: Int -> Line ()
landLeft n = L (\p@(l, r) -> if abs (l + n - r) < 4 then (Just (), (l + n, r)) else (Nothing, p))

landRight :: Int -> Line ()
landRight n = L (\p@(l, r) -> if abs (r + n - l) < 4 then (Just (), (l, r + n)) else (Nothing, p))

g :: Line ()                 --    (Just () (0, 0))
g = do landLeft     2        -- -> (Just () (2, 0)) 
       landRight    4        -- -> (Just () (2, 4))
       landLeft     (-1)     -- -> (Just () (1, 4))
       landRight    1        -- -> (Nothing () (1, 4))

execute :: (Maybe (), Pole)
execute = app g (0, 0)
