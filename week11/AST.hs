module AST where

import Result

type Identifier = String

data Expr = Lit Integer | Var Identifier | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
  deriving (Show)

infixl 6 :+: 
infixl 6 :-: 
infixl 7 :/:
infixl 7 :*:

-- eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Maybe a
-- eval (Lit k) _       = pure (fromInteger k)
-- eval (Var name) vars = lookup name vars
-- eval (x :+: y)  vars = pure (+) <*> eval x vars <*> eval y vars
-- eval (x :-: y)  vars = pure (-) <*> eval x vars <*> eval y vars
-- eval (x :*: y)  vars = pure (*) <*> eval x vars <*> eval y vars
-- eval (x :/: y)  vars = pure (/) <*> (eval x vars) <*> case (eval y vars) of 
--                                                       Just 0 -> Nothing
--                                                       z -> z

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a
eval (Lit k) _       = Okay (fromInteger k)
eval (Var name) vars = lookup' name vars
eval (x :+: y)  vars = pure (+) <*> eval x vars <*> eval y vars
eval (x :-: y)  vars = pure (-) <*> eval x vars <*> eval y vars
eval (x :*: y)  vars = pure (*) <*> eval x vars <*> eval y vars
eval (x :/: y)  vars = pure (/) <*> (eval x vars) <*> case (eval y vars) of 
                                                      Okay 0 -> Error ["division by zero"]
                                                      z -> z

lookup' :: Identifier -> [(Identifier,a)] -> Result a
lookup' el []          = Error [("unknown variable: " ++ el)]
lookup' el ((i,a):ias) = if el == i then Okay a else lookup' el ias
                                                    
