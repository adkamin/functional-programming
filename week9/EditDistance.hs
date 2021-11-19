module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1 + distance xs (y:ys), 1 + distance (x:xs) ys, cost x y + distance xs ys]

  cost x y = if x==y then 0 else 1


editDistance :: String -> String -> Int
editDistance xs ys = distArray ! (0, 0)
  where
  distance :: (Int,Int) -> Int
  distance (i,j)
    | i == length xs = length ys - j
    | j == length ys = length xs - i
    | otherwise = minimum [1 + distArray ! (i+1,j), 1 + distArray ! (i,j+1), cost (xs !! i) (ys !! j) + distArray ! (i+1,j+1)]
  
  cost x y = if x==y then 0 else 1
 
  distArray :: Array (Int,Int) Int
  distArray = array ((0,0), (length xs, length ys)) [(ij, distance ij) | i <- [0..length xs], j <- [0..length ys], let ij = (i,j)]

