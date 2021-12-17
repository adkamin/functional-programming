module Dice where

import System.Random
import RandomState
import RandomGen

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr 
          | Min Expr Expr | Max Expr Expr
          | Expr :-: Expr | Expr :/: Integer
  deriving (Show)

infixl 6 :+:
infixl 6 :-:
infixl 7 :/:

type DiceAction m = Integer -> m Integer


--evalM :: Expr -> DiceAction IO -> IO Integer             -- prototype
evalM :: (Monad m) => Expr -> DiceAction m -> m Integer  -- final version
evalM (Lit k) vars = return k
evalM (Dice k) vars = vars k
evalM (x :+: y) vars = return (+) <*> evalM x vars <*> evalM y vars
evalM (Min x y) vars= return (min) <*> evalM x vars <*> evalM y vars
evalM (Max x y) vars= return (max) <*> evalM x vars <*> evalM y vars
evalM (x :-: y) vars = return (-) <*> evalM x vars <*> evalM y vars
evalM (x :/: y) vars = return (div) <*> evalM x vars <*> return y


evalRIO :: Expr -> IO Integer
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Integer
evalIO expr = evalM expr dio
  where
  dio :: Integer -> IO Integer  
  dio i = do
    putStrLn $ "Result of dice" ++ show i ++ " roll"
    ip <- getLine
    let n = read ip
    if n <= i && n >= 1 then return n else dio i

evalND :: Expr -> [Integer]
evalND expr = evalM expr (\dice -> [1..dice])

avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)

evalR :: Expr -> RandomState Integer
evalR expr = evalM expr (\dice -> genRandInteger (1, dice)) 

observed :: (Fractional a) => Int -> Expr -> IO a
observed n expr = fmap avg (help n expr)
   where 
   help :: Int -> Expr -> IO [Integer]
   help 0 exprs = return []
   help n exprs = do
     g <- evalRIO exprs
     g2 <- help (n-1) exprs
     return (g : g2) 
