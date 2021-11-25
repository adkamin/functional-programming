module ThereCanBeOnlyOne where

onlyElem :: (Eq a) => a -> [a] -> Bool
onlyElem el []     = False
onlyElem el (x:xs)
    | el == x = True && not (onlyElem el xs)
    | el /= x = onlyElem el xs

onlyOnce :: (a -> Bool) -> [a] -> Bool
onlyOnce p []     = False 
onlyOnce p (x:xs)
    | p x == True  = True && not (onlyOnce p xs)
    | p x == False = onlyOnce p xs 

onlyElem' :: (Eq a) => a -> [a] -> Bool
onlyElem' el xs = onlyOnce (==el) xs