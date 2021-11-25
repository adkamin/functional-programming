module PolyTypes where

mingle :: [a] -> [a] -> [a]
mingle xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

sumWith :: (Num a) => (a -> a) -> [a] -> a
sumWith g xs = foldr (+) 0 (map g xs)

transform :: (a -> [b]) -> [a] -> [b]  -- concat takes list of lists as an argument
transform f  = concat . map f