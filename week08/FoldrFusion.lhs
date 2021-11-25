> module FoldrFusion where
>
> import Prelude hiding (map)

The fusion law for foldr states that

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f ==> foldr g e
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that f (g x y) = h x (f y) 

Which is the case since:
  
  foldr g e ((\x xs -> f x : xs) a b) 
= { applying property from 8.4: foldr f b xs = compose (map f xs) b . }
  compose (map g ((\x xs -> f x : xs) a b) e)
= { applying lambda }
  compose (map g (f a : b) e)
= { applying map }
  compose (((g (f a)) : map g b) e) 
= { applying compose }
  (g (f a)) compose (map g b) e
= { applying property from 8.4: foldr f b xs = compose (map f xs) b }
  (g (f a)) (foldr g e) b
= { definition of (.) }
  (g . f) a (foldr g e) b  

--------------------------------------
To prove:  map (f . g) = map f . map g
Using foldr fusion law: f . foldr g e = foldr h (f e), if for all x,y: f (g x y) = h x (f y)

  map (f . g) ys 
= { definition of map }
  foldr (\x xs -> (f . g) x : xs) [] ys
= { definition of (.) }
  foldr (\x xs -> (f (g x) : xs) [] ys
= { applying property (\x xs -> f x : xs) . g = (\x xs -> f (g x) : xs) }
  foldr ((\x xs -> f x : xs) . g) [] ys
= { applying (.) }
  foldr ((\x xs -> f x : xs) (g [])) ys     
= { applying foldr fusion, h = (\x xs -> f x : xs), g = f, e = [], f = map f }
  map f . foldr (\x xs -> f x : xs) [] ys
= { applying map, right-to-left }
  map f (map g xs)

----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

