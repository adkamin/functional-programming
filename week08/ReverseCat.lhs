> module ReverseCat where
> import Prelude hiding (reverse)
>
> reverse :: [a] -> [a]
> reverse [] = []
> reverse (x:xs) = reverse xs ++ [x]
>
> reverse' :: [a] -> [a]
> reverse' xs = reverseCat xs []
>
> reverseCat :: [a] -> [a] -> [a]
> reverseCat [] ys = ys
> reverseCat (x:xs) ys = reverseCat xs (x:ys)

---------------------------------------------
To prove: reverseCat xs ys = reverse xs ++ ys
By induction on xs.

Case 1: xs = []

  reverseCat [] ys
= { definition of reverseCat }
  ys
= { definition of ++ } 
  [] ++ ys
= { definition of reverse }
  reverse [] ++ ys

Case 2: xs = (a:as). IH: reverseCat as bs = reverse as ++ bs, for all bs.
  reverseCat (a:as) ys
= { definition of reverseCat }
  reverseCat as (a:ys)
= { IH, bs = (a:ys) }  
  reverse as ++ (a:ys)
= { definition of ++ and right-associativity of ++ }
  reverse as ++ a:(ys ++ [])
= { definition of ++ }
  reverse as ++ [a] ++ ys
= { definition of reverse }
  reverse (a:as) ++ ys
  

-----------------------------------------------------
To prove: reverse xs = reverse' xs
By induction on xs.

Case 1: xs = []

  reverse []
= { definition of reverse }
  []
= { definition of reverseCat (ys = [])}
  reverseCat [] []
= { definition of reverse' }
  reverse' []

Case 2: xs = (a:as). IH not needed

  reverse (a:as)
= { definition of ++ }
  reverse (a:as) ++ []
= { from property reverseCat xs ys = reverse xs ++ ys }
  reverseCat (a:as) []
= { definition of reverse' }
  reverse' (a:as)