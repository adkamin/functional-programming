module Monoids where

{- Determine if the following operations turn a type into a Monoid;
 - if so: give the corresponding value for the identity element

   The list cons operator,   (:) :: a -> [a] -> [a]
   mempty = []

   The boolean operator      (||) :: Bool -> Bool -> Bool
   mempty = False

   The function              mod :: (Integral a) => a -> a -> a
   mod is not associative

   The function              max :: (Ord a) => a -> a -> a
   we cannot define minBound

   Function composition,     (.) :: (b -> c) -> (a -> b) -> (a -> c)
   mempty = id

   The function              zipWith (+) :: [Int] -> [Int] -> [Int]
   mempty = [0..]

   The operator              (++/) :: [a] -> [a] -> [a] defined below
   not associative
  -}

(++/) :: [a] -> [a] -> [a]
[]     ++/ ys = ys
(x:xs) ++/ ys = x:(ys ++/ xs)

x : (y : z) = (x : y) : z