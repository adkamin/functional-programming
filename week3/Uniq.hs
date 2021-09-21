module Uniq where

--uniq :: (Eq a) => [a] -> [a]
uniq []       = []
uniq (x:y:ys) = if x == y then y : uniq ys else x : y : uniq ys
