module Empty where

{- which ones of these functions are the improper way to test for [] ? -}

-- not a good practice, since we need to run through the whole list to compare length to 0 (runs in O(n))
isEmpty0 list = length list == 0

-- not a good practice, since we want polymorphism but this enforces Eq class
isEmpty1 list = list == []

-- good practise, null is implemented using pattern matching
isEmpty2 list = null list

-- good practice, again thanks to pattern matching
isEmpty3 []   = True
isEmpty3 _    = False

someInts :: [Int]
someInts = [1..32]

manyInts :: [Int]
manyInts = [1..2^(27::Int)]

infiniteInts :: [Int]
infiniteInts = [1..]

nothingAtAll :: [a]
nothingAtAll = []

someFunctions :: [Int->Int->Int]
someFunctions = [(+), (*), mod, div]
