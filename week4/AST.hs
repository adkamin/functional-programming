module AST where

data Expr = 
    VarX          |
    Lit   Integer |
    Expr :+: Expr | 
    Expr :-: Expr | 
    Expr :*: Expr | 
    Expr :/: Expr 

infixl 6 :+:
infixl 7 :-:
infixl 8 :*:
infixl 9 :/:

eval :: (Fractional a) => Expr -> a -> a
eval (Lit i) _    = fromIntegral i
eval (VarX)  n    = n
eval (e1 :+: e2) n = eval e1 n + eval e2 n
eval (e1 :-: e2) n = eval e1 n - eval e2 n
eval (e1 :*: e2) n = eval e1 n * eval e2 n
eval (e1 :/: e2) n = eval e1 n / eval e2 n


--eval :: (Fractional a) => Expr -> a -> Maybe a
