module AST where

import System.Random
import RandomState
import Data.List
import RandomGen

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Integer -> m Integer


----------- 12.6.1, 12.6.2 ------------
evalM :: (Monad m) => Expr -> DiceAction m -> m Integer  -- final version
evalM (Lit i)     a = return i
evalM (Dice i)    a = a i
evalM (e1 :+: e2) a = pure (+)   <*> evalM e1 a <*> evalM e2 a
evalM (Min e1 e2) a = pure (min) <*> evalM e1 a <*> evalM e2 a
evalM (Max e1 e2) a = pure (max) <*> evalM e1 a <*> evalM e2 a


evalRIO :: Expr -> IO Integer
-- evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
 where report x = do { putStr "rolled a "; print x; return x }


--------- 12.6.3 -----------
evalIO :: Expr -> IO Integer
evalIO e = evalM e (\i -> do
  putStr "Result of dice roll?"
  input <- getLine
  let int = read input
  if int <= i then return int else evalIO (Dice i)) 


---------- 12.6.4 ------------
evalND :: Expr -> [Integer]
evalND e = evalM e (\i -> [1..i])


avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)


expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)


histogram = map (\x -> (head x, length x)) . group . sort . evalND


---------- 12.6.5 -----------
evalR :: Expr -> RandomState Integer
evalR e = evalM e (\i -> genRandInteger (1,i))

----------- 12.6.6 ----------
observed :: (Fractional a) => Int -> Expr -> IO a
observed n dice = do 
  throws <- throwMany n dice
  let average = avg throws
  return average


throwMany :: Int -> Expr -> IO [Integer]
throwMany 0 dice = return []
throwMany n dice = do
  x <- evalIO dice
  xs <- throwMany (n-1) dice
  return (x:xs)
