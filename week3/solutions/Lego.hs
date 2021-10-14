module Lego where

import Data.List

removeAt :: Int -> [a] -> [a] 
removeAt n xs = [ x | (i,x) <- zip [0..] xs, i /= n ]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort (zip xs [0..])

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = [ (x,i) | ((_,x),i) <- sortWithPos (map swap (sortWithPos xs)) ]
  where swap (x,y) = (y,x) 
