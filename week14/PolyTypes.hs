module PolyTypes where

lift0 :: (a -> b) -> a -> b
lift0 f x          = f x

lift1 :: (b -> c) -> (a -> b) -> a -> c  
lift1 f g1 x       = f (g1 x)

lift2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
lift2 f g1 g2 x    = f (g1 x) (g2 x)

deMorgan :: ((a -> Bool) -> (b -> Bool)) -> (a -> Bool) -> b -> Bool
deMorgan quantor p  = not . quantor (not . p)