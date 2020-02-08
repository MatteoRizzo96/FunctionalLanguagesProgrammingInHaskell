module Ex5_8 where

-- Ex 5
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
expr :: Parser Expr

-- Ex 8
expr :: Parser Int
expr term = do t <- term
               ts <- many (symbol "-" term)
               return foldl (-) t ts