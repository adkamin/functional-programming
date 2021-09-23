module Uniq where

uniq :: (Eq a) => [a] -> [a]
uniq []       = []
uniq [x]      = [x]
uniq (x:y:ys) = if x == y then y : uniq ys else x : uniq (y:ys)
