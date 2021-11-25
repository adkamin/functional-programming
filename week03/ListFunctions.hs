module ListFunctions where

import Prelude hiding (or, and, elem, take, drop)

and :: [Bool] -> Bool
and []     = True 
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or []     = False
or (x:xs) = if x then True else or xs

elem :: (Eq a) => a -> [a] -> Bool
elem el []     = False
elem el (x:xs) = if el == x then True else elem el xs

drop :: Int -> [a] -> [a]
drop _ []     = []
drop 0 xs     = xs
drop n (x:xs) = drop (n-1) xs

take :: Int -> [a] -> [a]
take _ []     = []
take 0 xs     = xs
take n (x:xs) = x : take (n-1) xs