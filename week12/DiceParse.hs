module ParseDice where

import Control.Applicative
import Parser
import Dice

{-

expr = fraction

fraction = formula
    | term, "/", positive

formula = formula, "+", term
    | formula, "-", term
    | term

term = "(", expr, ")"
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
formula = do
    t <- term 
    processTerm t

processTerm :: Expr -> Parser Expr
processTerm f = do
        s <- symbol "+"
        t <- term
        processTerm (f :+: t)
    <|> do
        s <- symbol "-"
        t <- term
        processTerm (f :-: t)
    <|> do
        return f

term :: Parser Expr
term = do
        symbol "("
        e <- expr
        symbol ")"
        return e
    <|>   
        diceParse    
    <|> 
        Lit <$> integer   

diceParse :: Parser Expr
diceParse = do
        symbol "d"
        p <- positive
        return (Dice p)
    <|> do
        n <- positive
        symbol "d"
        p <- positive
        return (addDice (fromInteger n) (Dice p))


addDice :: Int -> Expr -> Expr
addDice 1 d = d
addDice n d = d :+: addDice (n-1) d

positive :: Parser Integer
positive = do
    n <- nat
    if n == 0 then failure else return n

-- test cases: a list of tuples listing the input and output of "parseAll expr"
-- in case you used a different constructor for division, edit the "where" definitions
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
       , "d6/2"      =-> Just $ Dice 6 </> 2                          
       , "2/d6"      =-> Nothing                             
       , "1+2/3"     =-> Nothing                              
       , "1+(2/3)"   =-> Just $ Lit 1 :+: (Lit 2 </> 3)       
       , "(1+2)/3"   =-> Just $ (Lit 1 :+: Lit 2) </> 3       
       ]
  where (=->) = (,)
        infixr 0 =->
        (</>) = (:/:) -- change this if you used a different name for "division"

-- use this function to get all incorrect answers
deviations :: (Eq b) => (a->b) -> [(a,b)] -> [(a,b,b)]
deviations f ans = [ (x,y,f x) | (x,y) <- ans, f x /= y ]

-- combine with the functions in dice
calculate :: (Fractional a) => String -> Maybe a
calculate str = expectation <$> parseAll expr str
