module Reverse where

import Prelude hiding (reverse)

-- This definition traverses the list - 1 element each time, meaning it runs in O(n^2).
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- This definition runs in O(n). We start with an empty list and transfer elements to it from top to bottom.
reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev []     acc = acc
        rev (y:ys) acc = rev ys (y:acc)