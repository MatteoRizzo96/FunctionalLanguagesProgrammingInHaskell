module CoreLanguage where
import Control.Applicative
import Parser
import UtilityParsers

-- .............................................
--              Syntax definition
-- .............................................

type Name = String

data Expr a = EVar Name              -- Variables
            | ENum Int               -- Numbers
            | EConstr Int Int        -- Constructor tag arity
            | EAp (Expr a) (Expr a)  -- Applications
            | ELet                   -- Let(rec) for recursive expressions
                   IsRec             --     boolean (True = Recursive, False = NonRecursive)
                   [Def a]           --     Definitions (let and letrec)
                   (Expr a)          --     Body of the let(rec)
            | ECase                  -- Case Expressions
                   (Expr a)          --     Expression to scrutinise
                   [Alter a]         --     Alternatives
            | ELam [a] (Expr a)      -- Lambda abstractions
            deriving Show

-- List of keywords for the core-language
keywords = ["let", "letrec", "in", "case", "of", "where"]

-- Type for cases
type Alter a = (Int, [a], Expr a)

-- Type for let and letrec
type Def a = (a, Expr a)
data IsRec = NonRecursive | Recursive deriving (Eq, Show)

-- A core-language program is a list of supercombinator definitions
type Program a = [ScDef a]
type CoreProgram = Program Name

-- A supercombinator definition contains its name, arguments and body
type ScDef a = (Name, [a], Expr a)
type CoreScDefN = ScDef Name

-- .............................................
--              Utility functions
-- .............................................

-- Check if the parsing input is equal to a given string
character :: Name -> Parser Name
character = symbol

-- Build the return value of an expression
buildExpr :: Name -> Expr Name -> Expr Name -> Expr Name
buildExpr op e = EAp (EAp (EVar op) e)

-- Compress a list of atomic expressions into a single expression
compAExprs :: [Expr Name] -> Expr Name
compAExprs [e] = e
compAExprs es = EAp (compAExprs (init es)) (last es)

-- .............................................
--                 Main parsers
-- .............................................

{- Parse a core-language program.
   For example, the program in core-language syntax:

        main = double 21 ;
        double x = x + x

   Is parsed as:

       [("main"  , []   , (EAp (EVar "double") (ENum 21))),
        ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))]
-}
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

-- Parse a supercombinator definition (i.e. function definition)
parseScDef :: Parser (ScDef Name)
parseScDef = do v <- identifier
                pf <- many identifier
                character "="
                body <- parseExpr
                return (v, pf, body)

-- .............................................
--       Parsers for specific expressions
-- .............................................

-- Parse a variable: var - EVar Name
parseVar :: Parser (Expr Name)
parseVar = do var <- identifier
              if var `notElem` keywords then return (EVar var) else empty

-- Parse a number: num - ENum Int
parseNum :: Parser (Expr Name)
parseNum = do num <- integer
              return (ENum num)

-- Parse a relop distinguishing among the many possibilities
parseRelop :: Parser (Expr Name)
parseRelop = do rel <- relop
                return (EVar rel)

-- Definitions

-- Distinguish between recursive and non recursive definition (i.e. let/letrec)
parseRec :: Parser IsRec
parseRec = do character "letrec"
              return Recursive
              <|> do character "let"
                     return NonRecursive

-- Parse multiple definitions: def1; ... ; defN
parseDefs :: Parser [Def Name]
parseDefs = do d <- parseDef
               do character ";"
                  ds <- parseDefs
                  return (d:ds)
                  <|> return [d]

-- Parse a single definition: var = expr
parseDef :: Parser (Def Name)
parseDef = do v <- identifier
              character "="
              expr <- parseExpr
              return (v, expr)

-- Case alternatives

-- Parse multiple alternatives: alt1; ... ; altN (N >= 1)
parseAlts :: Parser [Alter Name]
parseAlts = do alt <- parseAlt
               do character ";"
                  alts <- parseAlts
                  return (alt:alts)
                  <|> return [alt]

-- Parse a single alternative: <num> var1 ... varN -> expr (N >= 0)
parseAlt :: Parser (Alter Name)
parseAlt = do character "<"
              n <- natural
              character ">"
              vars <- many identifier
              character "->"
              expr <- parseExpr
              return (n, vars, expr)

-- .............................................
--          General parsers for expressions
-- .............................................

{- Parse a complex expression in the order:
   1. Local (recursive) definitions  -> let/letrec defs in expr   - ELet
   2. Case expression                -> case expr of alts         - ECase
   3. Lambda abstraction (n >=1)     -> \ var1 ... varN . expr    - ELam
-}
parseExpr :: Parser (Expr Name)
parseExpr = do rec <- parseRec
               defs <- parseDefs
               character "in"
               expr <- parseExpr
               return (ELet rec defs expr)
            <|> do character "case"
                   expr <- parseExpr
                   character "of"
                   alts <- parseAlts
                   return (ECase expr alts)
            <|> do character "\\"
                   v <- identifier
                   vs <- many identifier
                   symbol "."
                   expr <- parseExpr
                   return (ELam (v:vs) expr)
            <|> parseExpr1

{- Parse an atomic expression in the order:
   1. Variable                 -> var              - EVar
   2. Number                   -> num              - ENum
   3. Constructor              -> Pack{num,num}    - EConstr
   4. Parenthesis expression   -> ( expr )
-}
parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar
             <|> parseNum
             <|> do character "Pack"
                    character "{"
                    n0 <- integer
                    character ","
                    n1 <- integer
                    character "}"
                    return (EConstr n0 n1)
             <|> do character "("
                    expr <- parseExpr
                    character ")"
                    return expr

-- .............................................
--      Parsers for expressions from 1 to 6
-- .............................................

{- Parse an "or" expression: expr1 -> expr2 | expr1
                                    | expr2
-}
parseExpr1 :: Parser (Expr Name)
parseExpr1 = do expr2 <- parseExpr2
                do character "|"
                   expr1 <- parseExpr1
                   return (buildExpr "|" expr2 expr1)
                   <|> return expr2

{- Parse an "and" expression: expr2 -> expr3 & expr2
                                     | expr3
-}
parseExpr2 :: Parser (Expr Name)
parseExpr2 = do expr3 <- parseExpr3
                do character "&"
                   expr2 <- parseExpr2
                   return (buildExpr "&" expr3 expr2)
                   <|> return expr3

{- Parse a "relop" expression: expr3 -> expr4 relop expr4
                                      | expr4
-}
parseExpr3 :: Parser (Expr Name)
parseExpr3 = do expr4fst <- parseExpr4
                do op <- parseRelop
                   expr4snd <- parseExpr4
                   return (EAp (EAp op expr4fst) expr4snd)
                   <|> return expr4fst

{- Parse a "+" or "-" expression: expr4 -> expr5 + expr4
                                         | expr5 - expr5
                                         | expr5
-}
parseExpr4 :: Parser (Expr Name)
parseExpr4 = do expr5fst <- parseExpr5
                do character "+"
                   expr4 <- parseExpr4
                   return (buildExpr "+" expr5fst expr4)
                   <|> do character "-"
                          expr5snd <- parseExpr5
                          return (buildExpr "-" expr5fst expr5snd)
                   <|> return expr5fst

{- Parse a "*" or "/" expression: expr5 -> expr6 * expr5
                                         | expr6 / expr6
                                         | expr6
-}
parseExpr5 :: Parser (Expr Name)
parseExpr5 = do expr6fst <- parseExpr6
                do character "*"
                   expr5 <- parseExpr5
                   return (buildExpr "*" expr6fst expr5)
                   <|> do character "/"
                          expr6snd <- parseExpr6
                          return (buildExpr "/" expr6fst expr6snd)
                   <|> return expr6fst

-- Parse an expression of the type: aexpr1 ... aexprN (N>=1)
parseExpr6 :: Parser (Expr Name)
parseExpr6 = do aexprs <- some parseAExpr
                return (compAExprs aexprs)
