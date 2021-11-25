module ShortCircuit where

andl, andr, orl, orr :: [Bool] -> Bool

andl = foldl (&&) True
andr = foldr (&&) True
orl  = foldl (||) False
orr  = foldr (||) False

e1, e2, e3, e4 :: Bool

e1 = andl $ False : [True,  True  ..] -- Does not terminate, because the list is forced to be evaluated
e2 = andr $ False : [True,  True  ..] -- Returns False immediatelly, because we already have the head without producing the rest
e3 = orl  $ True  : [False, False ..] -- Does not terminate, because the list is forced to be evaluated
e4 = orr  $ True  : [False, False ..] -- Returns True immediatelly, because we already have the head without producing the rest

{-
foldr :: (a → b → b) → b → [a] → b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

foldl :: (b → a → b) → b → [a] → b
foldl f b []     = b
foldl f b (x:xs) = foldl f (f b x) xs
-}