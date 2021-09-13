module Swap where

-- 1
--swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

--sumProduct :: (Int, Int) -> (Int, Int)
sumProduct (x,y) = (x+y,x*y)

--smallerFirst :: (Int, Int) -> (Int, Int)
smallerFirst (x,y) 
    | x < y     = (x,y)
    | otherwise = (y,x)

-- 2
{- 
Works for swap, but not for the remaining functions as there the types are in the same order within the tuple.
For sumProduct, we need Num a => (a, a) -> (a, a) to perform (+) and (*)
For smallerFirst, we need Ord a => (a, a) -> (a, a) to perform (<) 
-}

-- 3
{- 
ghci> :type swap
swap :: (b, a) -> (a, b)
ghci> :type sumProduct
sumProduct :: Num b => (b, b) -> (b, b)
ghci> :type smallerFirst
smallerFirst :: Ord b => (b, b) -> (b, b)
-}

-- 4
{-
(Int,(Char,Bool)) is a tuple containing another tuple as it's second element
(Int,Char,Bool) is a 3-tuple

-}

convert :: (Int,(Char,Bool)) -> (Int,Char,Bool)
convert (n, (c,b)) = (n,c,b)