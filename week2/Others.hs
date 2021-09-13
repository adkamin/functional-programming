-- Exercise from slide 23

f6 :: Int -> (Int -> Int)
f6 n = \m -> n + m

f7 :: (Int -> Int) -> Int
f7 f = 1

f8 :: a -> (a -> a)
f8 x = \y -> x

{-
f9 :: (a -> a) -> a
f9 = 
-}