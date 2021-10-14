module AST where

data Expr = Lit Integer | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Var 
infixl 6 :+: 
infixl 6 :-: 
infixl 7 :/:
infixl 7 :*:

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a 
eval (Lit k) _ = Just (fromInteger k) 
eval (x :+: y) k = case (eval x k, eval y k) of 
                     (Just x', Just y') -> Just (x'+y')
                     _ -> Nothing
                       
eval (x :-: y) k = case (eval x k, eval y k) of 
                     (Just x', Just y') -> Just (x'-y')
                     _ -> Nothing

eval (x :*: y) k = case (eval x k, eval y k) of
                     (Just x', Just y') -> Just (x'*y') 
                     _ -> Nothing 

eval (x :/: y) k = case (eval x k, eval y k) of 
                     (Just x', Just 0)  -> Nothing
                     (Just x', Just y') -> Just (x'/y') 
                     _ -> Nothing

eval Var k = Just k
