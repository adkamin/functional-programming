
module ParseD where

import Control.Applicative
import Parser
import Dice


{-

expr     = fraction

fraction = formula
         | term, "/", positive

formula  = formula, "+", term
         | formula, "-", term
         | term

term     = "(", expr, ")"
         | integer                  -- constants
         | [positive], "d", positive -- dice

positive = <an integer greater than 0>

-}

expr :: Parser Expr
expr = fraction

fraction :: Parser Expr
fraction = do
       t <- term
       symbol "/"
       p <- positive
       return (t :/: p)
       <|>
       formula


formula :: Parser Expr
formula = do { t1 <- term; build t1}
    where build :: Expr -> Parser Expr
          build t = do {symbol "+"; t2 <- term; build (t :+: t2)} <|> 
                    do {symbol "-"; t2 <- term; build (t :-: t2)} <|> 
                    return t

term :: Parser Expr
term = symbol "(" *> expr <* symbol ")" 
       <|> do {p1 <- positive; symbol "d"; p2 <- positive; return (helper p1 p2)}
       <|> do {symbol "d"; p2 <- positive; return (helper 1 p2)} 
       <|> Lit <$> integer
       where
       helper 1 p2 = Dice p2
       helper p1 p2 = helper (p1-1) p2 :+: Dice p2

positive :: Parser Integer
positive = do 
   n <- nat
   if n == 0 then failure else return n

-- test cases: a list of tuples listing the input and output of "parseAll expr"
-- in case you used a different constructor for division, edit the definition of "</>"
test :: [(String, Maybe Expr)]
test = [ ""          =-> Nothing
       , "2"         =-> Just $ Lit 2
       , "d6"        =-> Just $ Dice 6
       , "(d6)"      =-> Just $ Dice 6
       , "((d6))"    =-> Just $ Dice 6
       , "2d10"      =-> Just $ Dice 10 :+: Dice 10
       , "xkcd"      =-> Nothing
       , "d6+d8"     =-> Just $ Dice 6 :+: Dice 8
       , "d10-1"     =-> Just $ Dice 10 :-: Lit 1
       , "1+d2+d3"   =-> Just $ Lit 1 :+: Dice 2 :+: Dice 3
       , "6-5-4"     =-> Just $ Lit 6 :-: Lit 5  :-: Lit 4
       , "2/d6"      =-> Nothing
       , "1+2/3"     =-> Nothing
       , "d6/2"      =-> Just $ Dice 6 </> 2  -- if these don't compile, edit below!
       , "1+(2/3)"   =-> Just $ Lit 1 :+: (Lit 2 </> 3)
       , "(1+2)/3"   =-> Just $ (Lit 1 :+: Lit 2) </> 3
       ]
  where (=->) = (,)
        infixr 0 =->

        -- depending on how you defined division in "Expr", choose the right definition here:
        (</>) :: Expr -> Integer -> Expr
        (</>) = (:/:)             -- data Expr = ... | Expr :/: Int | ...
        --(</>) = Div             --             ... | Div Expr Int | ...
        --x </> y = x :/: Lit y   --             ... | Expr :/: Expr | ...
        --x </> y = Div x (Lit y) --             ... | Div Expr Expr | ...

-- use this function to get all incorrect answers
deviations :: (Eq b) => (a->b) -> [(a,b)] -> [(a,b,b)]
deviations f ans = [ (x,y,f x) | (x,y) <- ans, f x /= y ]

-- combine with the functions in dice
calculate :: (Fractional a) => String -> Maybe a
calculate str = expectation <$> parseAll expr str
