module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

bits :: Int -> [Int]
bits n = unfoldr (\s -> if s /= 0 then Just (s `mod` 2, s `div` 2) else Nothing) n

zip :: [a] -> [b] -> [(a,b)]
zip l r = unfoldr (\(a,b) -> if (length a /= 0 && length b /= 0) then Just ((head a, head b), (drop 1 a, drop 1 b)) else Nothing)  (l,r) 

take :: Int -> [a] -> [a]
take n l = unfoldr (\(i,s) -> if (i /= 0 && length s /= 0) then Just (head s, ((i-1),drop 1 s)) else Nothing) (n,l)

-- How to make primes stop, instead of making the list have upper bound?
primes :: [Integer]
primes = unfoldr (\(s:sx) -> if (sx) /= [] then Just (s, (filter (\n -> n `mod` s /= 0) sx)) else Nothing) [2..]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) l1 l2 = apo (\(xs, ys) -> if length xs /= 0 then Right ((head xs), ((drop 1 xs), ys)) else Left ys) (l1, l2)

[] ys = ys
(x:xs) ys  x : xs ++ ys 

insert :: (Ord a) => a -> [a] -> [a]
insert el l = apo (\(x, ys) -> if (length ys) == 0 then Left [x] else if x <= (head ys) then Left (x : (head ys) : (drop 1 ys)) else Right ((head ys), (x,(drop 1 ys))) ) (el,l) 
-- this one looks better and I thought it is the same but it throws exception non-exhaustive pattern when inserting largest element:
-- insert el l = apo (\(x, (y:ys)) -> if (length (y:ys)) == 0 then Left [x] else if x <= y then Left (x : y : ys) else Right (y, (x,ys)) ) (el,l) 

-- unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
-- unfoldrApo grow seed = apo (\g s -> if g s == (Just (?,?)) then (Right (?,?)) else (Left ?)) grow seed

-- unfoldr :: (s -> Maybe (a,s)) -> s -> [a]
-- unfoldr grow seed = case grow seed of
--     Just (a, new_seed) -> a : unfoldr grow new_seed
--     Nothing -> []