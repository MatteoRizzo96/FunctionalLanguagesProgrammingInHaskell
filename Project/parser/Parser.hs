module Parser where
import Control.Applicative

-- Make Parser into a new type so that it can implement functors applicatives and monads
newtype Parser a = P (String -> [(a, String)])

-- Remove the dummy constructor
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- Functor
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            []          -> []
                            [(v, out)]  -> [(g v, out)])

-- Applicative
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                            [] -> []
                            [(g, out)] -> parse (fmap g px) out)

-- Monad
instance Monad Parser where
    -- return :: a -> Parser a
    return = pure
    
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            []           -> []
                            [(v, out)]   -> parse (f v) out)
-- Alternative
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            []          -> parse q inp
                            [(v, out)]  -> [(v, out)])
