module ListComprehensions where

import Prelude hiding (replicate, take, concat) 

{- note: "fully polymorphic" is a bit of a trick question: if a function has signature "Int -> a -> [a]", it is
 - not overloaded, but also not "fully" polymorphic due to the presence of 'Int'; depending on how you define "fully"
 - so either answer can be correct in that case -} 

-- this one is clearly fully polymorphic 
cartProd          :: [a] -> [b] -> [(a,b)] 
cartProd as bs    = [ (a,b) | a <- as, b <- bs ] 

-- it's okay to have "Int" or "Integer" instead of n; in that case the function is not overloaded, otherwise it is 
replicate          :: (Num n, Enum n) => n -> a -> [a]
replicate n y     = [ y | _i <- [1..n] ]

-- it's okay to have "Int" or "Integer" instead of n; in that case the function is not overloaded, otherwise it is
take              :: (Ord n, Num n, Enum n) => n -> [a] -> [a] 
take n xs         = [ x | (i,x) <- zip [0..] xs, i < n ]

-- this one is clearly overloaded; it is also okay to have "(Eq a, Num n, Enum n) => a -> [a] -> [n]"
findIndices       :: (Eq a) => a -> [a] -> [Int]
findIndices a xs  = [ i | (i,x) <- zip [0..] xs, x == a] 

-- this one is clearly fully polymorphic (and very similar to "++/") 
zipperList        :: [a] -> [a] -> [a] 
zipperList xs ys  = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

-- this one is clearly fully polymorphic 
concat            :: [[a]] -> [a]
concat xss        = [ x | xs <- xss, x <- xs ]
