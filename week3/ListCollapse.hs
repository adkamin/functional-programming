module ListCollapse where

import Prelude hiding ((++))

-- 1
concatr :: [[a]] -> [a]
concatr []     = []
concatr (x:xs) = x ++ concatr xs

-- 2
concatl :: [[a]] -> [a]
concatl []       = []
concatl xs       = res xs ++ last xs 
    where res ys = concatl (take ((length ys) - 1) ys)