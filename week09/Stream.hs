module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (x:>xs) = x

tail :: Stream a -> Stream a
tail (x:>xs) = xs

repeat :: a -> Stream a
repeat x = x :> repeat x

map :: (a -> b) -> (Stream a -> Stream b)
map f (x:>xs) = f x :> map f xs

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (x:>xs) (y:>ys) = f x y :> zipWith f xs ys

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (x:>xs) = if p x then x :> (filter p xs) else filter p xs

toList :: Stream a -> [a]
toList (x:>xs) = x : toList xs

cycle :: [a] -> Stream a
cycle xs = foldr (:>) (cycle xs) xs

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

primesList :: Stream Integer
primesList = cycle (sieve [2..])
  where sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p /= 0 ]

primetwins :: Stream (Integer,Integer)
primetwins = filter (\(p1,p2) -> (p1 + 2 == p2)) (zipWith (tuple) primes (tail primes))
  where tuple x y = (x,y)

primesFrom :: Integer -> Stream Integer
primesFrom n = filter (\i -> i >= n) primes

combine :: Stream a -> Stream a -> Stream a
combine (x:>xs) (y:>ys) = x :> y :> combine xs ys