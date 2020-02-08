module PrettyPrinter where
import CoreLanguage

-- ........................................
--         Implementation of iseq
-- ........................................

-- Implementation of iseq
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

-- The empty iseq
iNil :: Iseq
iNil = INil

-- Append two iseq
iAppend :: Iseq -> Iseq -> Iseq
iAppend = IAppend

-- Turn a string into an iseq
iStr :: String -> Iseq
iStr = IStr

-- Maps a number to an iseq
iNum :: Int -> Iseq
iNum n = iStr (show n)

-- Indent an iseq
iIndent :: Iseq -> Iseq
iIndent = IIndent

-- New line with indentation
iNewline :: Iseq
iNewline = INewline

-- Turn an iseq into a string
iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

-- Represent spaces
iSpace :: Iseq
iSpace = iStr " "

-- Flatten the printing
flatten :: Int -> [(Iseq, Int)] -> String
flatten col ((INewline, indent) : seqs)          = "\n" ++ spaces indent ++ flatten indent seqs
flatten col ((IIndent seq, indent) : seqs)       = flatten col ((seq, col) : seqs)
flatten col ((IStr s, indent) : seqs)            = s ++ flatten (col + length s) seqs
flatten col ((INil, indent) : seqs)              = flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col []                                   = ""

-- ........................................
--         Main pretty printers
-- ........................................

-- Main function
pprint :: Program Name -> String
pprint prog = iDisplay (pprProgram prog)

-- Pretty print programs
pprProgram :: Program Name -> Iseq
pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) (map pprSc prog)

-- Pretty print super combinators
pprSc :: ScDef Name -> Iseq
pprSc (name, args, body) = iConcat [iStr name, iSpace, pprArgs args, iStr " = ", iIndent (pprExpr body)]

-- ........................................
--            Utility functions
-- ........................................

-- Use iAppend to concatenate a list of iseqs into a single iseq
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

-- Similar to iConcat except that it interleaves a specified iseq between each adjacent pair
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep []         = iNil
iInterleave sep [seq]      = seq
iInterleave sep (seq:seqs) = seq `iAppend` (sep `iAppend` iInterleave sep seqs)

-- Return a list of n spaces
spaces :: Int -> String
spaces n = replicate n ' '

-- Check if an expression is atomic
isAtomicExpr :: Expr Name -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- ........................................
--        Specific pretty printers
-- ........................................

-- Pretty prints multiple definitions
pprdefs :: [(Name, Expr Name)] -> Iseq
pprdefs defs = iInterleave sep (map pprdef defs) where sep = iConcat [iStr ";", iNewline]

-- Pretty prints a single definition
pprdef :: (Name, Expr Name) -> Iseq
pprdef (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

-- Pretty prints arguments
pprArgs :: [String] -> Iseq
pprArgs args = iInterleave iSpace (map iStr args)

-- ........................................
--     Pretty printers for expressions
-- ........................................

-- Pretty print complex expressions with infix notation
pprExpr :: Expr Name -> Iseq
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp (EAp (EVar "+") e1) e2)  = iConcat [pprAExpr e1, iStr " + ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "-") e1) e2)  = iConcat [pprAExpr e1, iStr " - ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "*") e1) e2)  = iConcat [pprAExpr e1, iStr " * ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "/") e1) e2)  = iConcat [pprAExpr e1, iStr " / ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "<") e1) e2)  = iConcat [pprAExpr e1, iStr " < ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "<=") e1) e2) = iConcat [pprAExpr e1, iStr " <= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "==") e1) e2) = iConcat [pprAExpr e1, iStr " == ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "~=") e1) e2) = iConcat [pprAExpr e1, iStr " ~= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar ">=") e1) e2) = iConcat [pprAExpr e1, iStr " >= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar ">") e1) e2)  = iConcat [pprAExpr e1, iStr " > ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "&") e1) e2)  = iConcat [pprAExpr e1, iStr " & ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "|") e1) e2)  = iConcat [pprAExpr e1, iStr " | ", pprAExpr e2]
pprExpr (EAp e1 e2)                   = iConcat [pprExpr e1, iSpace, pprAExpr e2]
pprExpr (EConstr n1 n2)               = iConcat [iStr "Pack{", iNum n1, iStr ",", iNum n2, iStr "}"]
pprExpr (ELet isRec defs expr)        = iConcat [iStr keyword, iNewline, iStr " ", iIndent (pprdefs defs), iNewline, iStr "in ", pprExpr expr]
                                        where keyword = if isRec == Recursive then "letrec" else "let"
pprExpr (ECase e alts)                = iConcat [iStr "case ", pprExpr e, iStr " of", iNewline, iStr " ", iIndent (iInterleave iNl (map pprAlt alts))]
                                        where iNl = iConcat [iStr ";", iNewline]
                                              pprAlt (tag, args, rhs) = iConcat [iStr "<", iNum tag, iStr "> ", pprArgs args, iStr " -> ", iIndent (pprExpr rhs)]
pprExpr (ELam args body)              = iConcat [iStr "(\\", pprArgs args, iStr ". ", iIndent (pprExpr body), iStr ")"]

-- Pretty print expressions with parenthesis
pprAExpr :: Expr Name -> Iseq
pprAExpr e = if isAtomicExpr e then pprExpr e else iConcat [iStr "(", pprExpr e, iStr ")"]
