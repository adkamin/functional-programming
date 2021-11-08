(.) :: (b → c) → (a → b) → a → c
(f . g) x = f (g x)                  (0)

(++) :: [a] → [a] → [a]              
[] ++ ys = ys                        (1)
(x:xs) ++ ys = x : (xs ++ ys)        (2)

map :: (a → b) → [a] → [b]
map f [] = []                        (3)
map f (x:xs) = f x : map f xs        (4)

concat :: [[a]] → [a]
concat [] = []                       (5)
concat (x:xs) = x ++ concat x        (6)

-----------------------------------------------------
To prove: map (f . g) xs = map f (map g xs)
By induction on xs.

Case 1: xs = []

  map (f . g) []
= { 3 }
  []
= { 3, right-to-left }
  map f [] 
= { 3, right-to-left }
  map f (map g [])

Case 2: xs = (a:as)
IH: map (f . g) as = map f (map g as), for all f and g

  map (f . g) (a:as)
= { 4 }
  (f . g) a : map (f . g) as
= { IH }
  (f . g) a : map f (map g as)
= { 1, right-to-left }
  (f . g) a : (map f . map g) as
= {  }
  f (g a) : (map f . map g) as
= {  }

  map (f . g) 

  (map f . map g) (a:as)
  
  map f (map g (a:as))


-----------------------------------------------------
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
By induction on ...

Case 1: ...

Case 2: ...
IH: ...

-----------------------------------------------------
To prove: concat (map (map f) xs) = map f (concat xs)

...

