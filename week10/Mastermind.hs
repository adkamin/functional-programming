module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue 
  deriving (Eq, Ord, Enum, Show, Read)

type Code = [Colour]

scoreAttempt :: (Ord a) => [a] -> [a] -> (Int,Int)
scoreAttempt code guess = (rightPos, wrongPos)
  where 
    rightPos = length (filter (\(a,b) -> a == b) (zip code guess))
    wrongPos = colorsWrongPos code guess - rightPos

colorsWrongPos :: (Ord a) => [a] -> [a] -> Int
colorsWrongPos code []     = 0
colorsWrongPos code (g:gs) = if elem g code then 1 + colorsWrongPos (removeFirst g code) gs else colorsWrongPos gs code
  where 
    removeFirst el (x:xs) = if x == el then xs else x : removeFirst el xs
    removeFirst el []     = []

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)

{---------------------- IO parts -------------------------------}

getCode :: Int -> IO Code
getCode 0 = return []
getCode n = do
  r <- randomRIO (0,7)
  rs <- getCode (n-1)
  return (toEnum r:rs)

playGame :: Int -> Code -> IO ()
playGame 0 code = putStrLn "No more tries, game over"
playGame n code = do
  putStrLn ("Try to guess the secret code word, " ++ show n ++ " tries left")
  input <- getLine
  let (i,j) = scoreAttempt code (map read $ words input)
  if i == length code then putStrLn "Correct" else do 
    putStrLn ("Incorrect\n" ++ show i ++ " colour(s) in the correct position,\n" ++ (show j ++ " colour(s) in the wrong position."))
    playGame (n-1) code
  
main :: IO ()
main = do
  code <- getCode 4
  putStrLn ("I picked a random code word with " ++ (show (length code)) ++ " colors")
  putStrLn ("Possible colours are White Silver Green Red Orange Pink Yellow Blue.")
  playGame 1 code
