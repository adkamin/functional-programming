module Swap where

-- 1
--swap :: (Int, Int) -> (Int, Int)
swap (x,y) = (y,x)

--sumProduct :: (Int, Int) -> (Int, Int)
sumProduct (x,y) = (x+y,x*y)

--smallerFirst :: (Int, Int) -> (Int, Int)
smallerFirst (x,y) 
    | x < y     = (x,y)
    | otherwise = (y,x)

-- 2
{- 
Works for swap, because that one is polymorphic - works for any type of pairs
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
(Int,(Char,Bool)) is a nested tuple (contains a tuple as it's second element)
(Int,Char,Bool) is a triple
-}

convert :: (Int,(Char,Bool)) -> (Int,Char,Bool)
convert (n, (c,b)) = (n,c,b)