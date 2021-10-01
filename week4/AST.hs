module AST where

import Data.Maybe

data Expr = 
    VarX          |
    Lit   Integer |
    Expr :+: Expr | 
    Expr :-: Expr | 
    Expr :*: Expr | 
    Expr :/: Expr 

infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit i) _     = Just (fromIntegral i)
eval (VarX)  n     = Just n
eval (e1 :+: e2) n = case (eval e1 n, eval e2 n) of 
                        (Just x, Just y) -> Just (x + y)
                        (_,_)             -> Nothing
eval (e1 :-: e2) n = case (eval e1 n, eval e2 n) of
                        (Just x, Just y) -> Just (x - y)
                        (_,_)            -> Nothing
eval (e1 :*: e2) n = case (eval e1 n, eval e2 n) of
                        (Just x, Just y) -> Just (x * y)
                        (_,_)            -> Nothing
eval (e1 :/: e2) n = case (eval e1 n, eval e2 n) of
                        (Just x, Just 0)   -> Nothing
                        (Just x, Just y)   -> Just (x / y)
                        (_,_)              -> Nothing