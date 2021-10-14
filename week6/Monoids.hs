module Monoids where

{- Determine if the following operations turn a type into a Monoid;
 - if so: give the corresponding value for the identity element

   The list cons operator,   (:) :: a -> [a] -> [a]
   mempty = []

   The boolean operator      (||) :: Bool -> Bool -> Bool
   mempty = False

   The function              mod :: (Integral a) => a -> a -> a
   mempty = (\y -> y `mod` + 1)

   The function              max :: (Ord a) => a -> a -> a
   mempty = minBound (if a is also of class Bounded)

   Function composition,     (.) :: (b -> c) -> (a -> b) -> (a -> c)
   mempty = id

   The function              zipWith (+) :: [Int] -> [Int] -> [Int]
   mempty = (\l -> replicate 0 (length l))

   The operator              (++/) :: [a] -> [a] -> [a] defined below
   ?
  -}

(++/) :: [a] -> [a] -> [a]
[]     ++/ ys = ys
(x:xs) ++/ ys = x:(ys ++/ xs)
