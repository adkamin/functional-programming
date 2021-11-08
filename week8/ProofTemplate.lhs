Local definitions:

> import Prelude hiding (++)
> 
> (++) :: [a] -> [a] -> [a]
> []     ++ ys = ys
> (x:xs) ++ ys = x : xs++ys

1.1
Monoid laws: 
left-identity:  [] ++ xs = xs                            (follows directly from ++)
right-identity: xs ++ [] = xs                            (needs induction)
associativity:  xs ++ (ys ++ zs) = (xs ++ ys) ++ zs      (needs induction)


2.2
-----------------------------------------------------
To prove: xs ++ [] = xs
By induction on xs.

Base case: xs = []:

  [] ++ []
= { definition of ++ }
  []

Inductive case: xs = (a:as). IH: as ++ [] = as

  (a:as) ++ [] 
= { definition of ++ }
  a : (as ++ [])
=  { IH }
  a : as

-----------------------------------------------------
To prove: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
By induction on xs.

Base case: xs = []:

  [] ++ (ys ++ zs)
= { definition of ++ }
  (ys ++ zs)
= { definition of ++ }
  ([] ++ ys) ++ zs

Inductive case: xs = (a:as). IH: as ++ (ys ++ zs) = (as ++ ys) ++ zs
  (a:as) ++ (ys ++ zs) 
= { definition of ++ }
  a : as ++ (ys ++ zy)
= { IH }
  a : (as ++ ys) ++ zs
  { definition of ++ }
  ((a:as) ++ ys) ++ zs

