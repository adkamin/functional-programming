module FindMinimum where

import Data.List

-- 9.2.1

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let e = minimum xs in e : selectionSort (delete e xs)

leastElem :: (Ord a) => [a] -> a
leastElem = head . selectionSort

{-
In call by value evaluation, the whole list needs to be sorted before we get the minimum.
In call by name evaluation, we would just take head of the list once we have the head 
(and the tail does not need to be processed.) Because the first iteration of selectionSort
finds the smallest element and puts it in the start, we know that we can take the head 
already after the first iteration.
-}

-- 9.2.2

{-
Because Haskell has lazy evalutation, running leastElem is instant as 0 is returned immediatelly
after the first iteration of selectionSort, but running selectionSort takes a lot of time, because
we are sorting a large list and we can only stop once the whole list is processed.
-}

-- 9.2.3

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
{- note: insert is in the Prelude, defined as:
  insert a [] = [a]
  insert a (b:xs)
    | a <= b    = a : b : xs
    | otherwise = b : insert a xs
-}

leastElem' :: (Ord a) => [a] -> a
leastElem' = head . insertionSort

someNums :: [Int]
someNums = [ 257*x `mod` 1337 | x <- [1..100000] ]

{-
insertionSort is worse, because we are forced to evaluate the whole list when calling insert.
This means that the list needs to get sorted before we can return minimum (head of the list).
-}