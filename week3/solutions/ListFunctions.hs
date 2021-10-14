module ListFunctions where

import Prelude hiding (or, and, elem, take, drop)

and :: [Bool] -> Bool
and [] = True 
and (x:xs) = x && and xs 

or :: [Bool] -> Bool
or [] = False 
or (x:xs) = x || or xs

elem :: (Eq a) => a -> [a] -> Bool 
elem _ [] = False
elem e (x:xs) = e == x || elem e xs

drop :: Int -> [a] -> [a] 
drop _ [] = []
drop n list@(_:xs) 
  | n <= 0 = list 
  | otherwise = drop (n-1) xs

take :: Int -> [a] -> [a] 
take _ [] = [] 
take n (x:xs) 
  | n <= 0 = []
  | otherwise = x : take (n-1) xs 
