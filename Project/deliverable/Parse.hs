module Parse where
import Control.Applicative
import Data.Char

-- .............................................
--          Definition of the Parser
-- .............................................

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

-- .............................................
--              Utility parsers
-- .............................................

-- Parse an identifier (i.e. the binding of a variable)
ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

-- Parse a natural number
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- Parse a space
space :: Parser ()
space = do many (sat isSpace)
           return ()    -- empty tuple as dummy result value

-- Parse an integer number
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat

-- Parse the next token ignoring spaces
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Parses the first item of a string
item :: Parser Char
item = P (\inp -> case inp of
                       []     -> []
                       (x:xs) -> [(x, xs)])

-- Check if the input property is satisfied by the parsed character
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- Check if the parsed character is a digit
digit :: Parser Char
digit = sat isDigit

-- Check if the parsed character is lower case
lower :: Parser Char
lower = sat isLower

-- Check if the parsed character is upper case
upper :: Parser Char
upper = sat isUpper

-- Check if the parsed character is a letter
letter :: Parser Char
letter = sat isAlpha

-- Check if the parsed character is alphanumeric
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- Check if the parsed character is alphanumeric
char :: Char -> Parser Char
char x = sat (== x)

-- Check if the input contains the given string and parse it
string :: String -> Parser String
string []       = return []
string (x:xs)   = do char x
                     string xs
                     return (x:xs)

-- Parse an identifier
identifier :: Parser String
identifier = token ident

-- Parse a natural number
natural :: Parser Int
natural = token nat

-- Parse an integer number
integer :: Parser Int
integer = token int

-- Parse a symbol
symbol :: String -> Parser String
symbol xs = token (string xs)

-- Parse a relop
relop :: Parser String
relop = symbol "=="
    <|> symbol "~="
    <|> symbol ">"
    <|> symbol ">="
    <|> symbol "<"
    <|> symbol "<="

-- Parser for non empty list of natural numbers
nats :: Parser [Int]
nats = do symbol "["
          n  <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)
