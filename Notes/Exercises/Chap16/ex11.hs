module Ex11 where

-- comp' e c = comp e ++ c

comp'::Expr -> Code -> Code
-- Base case
comp' (Val n) c 
    = -- {apply comp'}
    comp(Val n) ++ c
    = -- {apply comp}
    [PUSH n] ++ c
    = -- {apply ++}
    PUSH n:c

-- Inductive case
comp' (Add x y) c
    = -- {def of comp'}
    comp(Add x y) ++ c
    = -- {apply comp}
    comp x ++ comp y ++ [ADD] ++ c
    = -- {associativity of ++}
    comp x ++ (comp y ++ ([ADD] ++ c))
    = -- {apply ++}
    comp x ++ (comp y ++ (ADD:c))
    = -- {induction on x}
    comp' x (comp y ++ (ADD:c))
    = -- {induction on y}
    comp' x (comp' y (ADD:c))