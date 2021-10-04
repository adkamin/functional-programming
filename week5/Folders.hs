module Folders where

import Prelude hiding (and,or,elem,maximum)

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

elem :: (Eq a) => a -> [a] -> Bool
elem el = foldr (\x xs -> el == x || xs) False

maximum :: (Ord a) => [a] -> a
maximum [] = error "cannot find maximum of empty list"
maximum xs = foldr (max) (last xs) xs

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr (\x -> insert x) Leaf

fromBits :: [Integer] -> Integer
fromBits = foldl (\acc x -> x + 2 * acc) 0

{-
Works, thanks to the pattern:
[1,0,1,0,1,0] = 2×(2×(2×(2×(2×1+0)+1)+0)+1)+0
              = 32×1 + 16×0 + 8×1 + 4×0 + 2×1 + 1×0
              = 42
-}


{- -------------------------------------------------------------------}

-- the relevant definitions for 'fromList'

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x tree@(Node key lt rt)
  | x < key   = Node key (insert x lt) rt
  | x > key   = Node key lt (insert x rt)
  | otherwise = tree
