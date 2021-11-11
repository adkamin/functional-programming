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
= { 0 }
  (f (g a)) : map f (map g as)
= { 4 }
  map f (g a : (map g as)
= { 4, right-to-left }
  map f (map g (a:as))


-----------------------------------------------------
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
By induction on as (as this is what we pattern match on in (++))

Case 1: as = []

  map f ([] ++ bs)
= { 3 }
  map f (bs)
= { 1, right-to-left }
  [] ++ map f bs
= { 3, right-to-left }
  (map f []]) ++ (map f bs)


Case 2: as = c:cs
IH: map f (cs ++ bs) = (map f cs) ++ (map f bs)

  map f ((c:cs) ++ bs)
= { 4 }
  f c : map f (cs ++ bs)
= { IH }
  f c : (map f cs) ++ (map f bs)
= { 4, right-to-left }
  (map f (c:cs)) ++ (map f bs)

-----------------------------------------------------
To prove: concat (map (map f) xs) = map f (concat xs)
By induction on xs

Case 1: xs = []

  concat (map (map f) [])
= { 3 }
  concat []
= { 5 }
  []
= { 3, right-to-left}
  map f []
= { 5, right-to-left }
  map f (concat [])

Case 2: xs = a:as. 
IH: concat (map (map f) as) = map f (concat as)

  concat (map (map f) (a:as))
= { 4 }
  concat ((map f) a : map (map f) as)
= { 6 }
  map f a : concat (map (map f) as)
= { IH }
  map f a : map f (concat as)
= { 4 and identity of (:) }
  f a : [] : map f (concat as)
= { definition of (:) }
  f a : map f (concat as)
= { 4, right-to-left }
  map f (a : concat as)
= { 2 }
  map f (a ++ concat as)
= { 6, right-to-left }
  map f (concat (a:as))
