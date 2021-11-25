Local definitions:

> import Prelude hiding (foldr)

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr f b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)
>
> compose :: [a -> a] -> a -> a
> compose [] = id
> compose (f:fs) = f . compose fs

compose :: [a -> a] -> a -> a        
compose [] b     = b                 (9)
compose (f:fs) b = f (compose fs) b  (10)

-----------------------------------------------------
To prove: foldr f b xs = compose (map f xs) b
By induction on xs.

Case 1: xs = []

  foldr f b []
= { 7 }
  b
= { 9 }
  compose [] b
= { 3, right-to-left }
  compose (map f []) b

Case 2: xs = a:as
IH: foldr f b as = compose (map f as) b

  foldr f b (a:as)
= { 8 }
  f a (foldr f b as)
= { IH }
  f a (compose (map f as) b)
= { 10 }
  compose (f a : map f as) b
= { 4, right-to-left }
  compose (map f (a:as)) b
