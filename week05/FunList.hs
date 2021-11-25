module FunList where

compose :: [a -> a] -> (a -> a)
compose [f] x    = f x
compose (f:fs) x = f (compose fs x)

compose' :: [a -> a] -> (a -> a)
compose' l x = foldr (\f fs -> f fs) x l

-- Computes the factorial of number n
-- first we compute map, which results in a list of n number of partial functions, [(*1),(*2),...(*n)]
-- then we use compose, which puts the functions together, starting with 1 * (compose [(*2),...(*n)] 1)
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' st ba xs = compose (map st xs) ba  
