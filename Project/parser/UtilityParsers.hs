module UtilityParsers where
import Parser
import Control.Applicative
import Data.Char


-- (1) Parsers that perform intermediate processing

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

-- (2) Parsers that check the satisfaction of some condition

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

-- (3) Parsers that perform the final processing

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

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
