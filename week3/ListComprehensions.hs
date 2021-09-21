module ListComprehensions where

import Data.List

-- creates all possible combinations taking an element from each list
combinations :: [a] -> [b] -> [(a,b)]                       -- polymorphic
combinations as bs = [ (a,b) | a <- as, b <- bs ]

-- creates a list of n replicated y times
replicate' :: (Num a, Enum a) => a -> b -> [b]              -- overloaded
replicate' n y   = [ y | i <- [1..n] ]

-- takes first n elements of list xs
take' :: (Num a, Enum a, Ord a) => a -> [b] -> [b]          -- overloaded
take' n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

-- returns the element before the first element equal to a
previous :: (Num a, Enum a, Eq b) => b -> [b] -> [a]        -- overloaded
previous a xs  = [ i | (i,x) <- zip [0..] xs, x == a]

-- interchanges elements from two lists
interchange :: [a] -> [a] -> [a]                            -- polymorphic
interchange xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

-- concatenates a list of lists
concat' :: [[a]] -> [a]                                     -- polymorphic
concat' xss   = [ x | xs <- xss, x <- xs ]
