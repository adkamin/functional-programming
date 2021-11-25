module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt n xs = [x | (x,i) <- zip xs [1..], i /= n]
-- Alternative: 
-- removeAt n xs = take (n-1) xs ++ reverse (take n (reverse xs))

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort [(x,i) | (x,i) <- zip xs [0..]]

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = removeDups [(s,i) | s <- xs, (x,i) <- l, x == s]
    where l = (zip (sort xs) [0..])

removeDups :: (Eq a) => [a] -> [a]
removeDups xs = [x | (i,x) <- zip [0..] xs, notElem x (take i xs)]