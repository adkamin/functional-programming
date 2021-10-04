module HOF where

import Prelude hiding (const)

{- exercise 5.1 -}

const :: a -> b -> a
const x y   = x

($->) :: a -> (a -> b) -> b
x $-> y      = y x

oper :: (Fractional a) => String -> a -> (a -> a)
oper "mul" n = (*n)
oper "div" n = (n/)
oper _     _ = error "not implemented"

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f xs  = map (map f) xs

without :: (a -> Bool) -> [a] -> [a]
without p    = filter (not . p)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c      
on f g x y   = f (g x) (g y)

{- exercise 5.2 -}

-- Adds 1 to a number and then multiplies by 5
f1 :: (Num a) => a -> a
f1 = (* 5) . (+ 1)

-- Multiplies a number by 5 and then adds 1
f2 :: (Num a) => a -> a
f2 = (+ 1) . (* 5)

-- Gets a maximum of a number and 0, (say m) and then gets a minimum of m and 100
f3 :: (Num a, Ord a) => a -> a
f3 = (min 100) . (max 0)

-- Checks if length of a list is smaller than 5
f4 :: [a] -> Bool
f4 = (<5) . length

-- If we change f4 to f4 = (5<) . length, we the function then checks if length of a list is greater than 5.
-- If we change f4 to f4 = ((<)5) . length, it is equivalent to \x -> 5 < x, so the function again checks if length of a list is greater than 5.