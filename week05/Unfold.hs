module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

bits :: Int -> [Int]
bits n = unfoldr (\s -> if s /= 0 then Just (s `mod` 2, s `div` 2) else Nothing) n

zip :: [a] -> [b] -> [(a,b)]
zip l r = unfoldr (\(a,b) -> if (length a /= 0 && length b /= 0) then Just ((head a, head b), (tail a, tail b)) else Nothing)  (l,r) 

take :: Int -> [a] -> [a]
take n l = unfoldr (\(i,s) -> if (i /= 0 && length s /= 0) then Just (head s, ((i-1),tail s)) else Nothing) (n,l)

-- take n primes does not work...?
primes :: [Integer]
primes = unfoldr (\s -> Just (head s, (filter (\n -> n `mod` (head s) /= 0) (tail s)))) [2..]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) l1 l2 = apo (\(xs, ys) -> if length xs /= 0 then Right ((head xs), ((tail xs), ys)) else Left ys) (l1, l2)

insert :: (Ord a) => a -> [a] -> [a]
insert el l = apo (\(x, ys) -> if (length ys) == 0 then Left [x] else if x <= (head ys) then Left (x : (head ys) : (tail ys)) else Right ((head ys), (x,(tail ys))) ) (el,l) 

unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo grow = apo (leftRight . grow)
    where 
        leftRight (Just x) = Right x
        leftRight Nothing  = Left []