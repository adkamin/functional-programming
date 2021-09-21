module Others where

-- Pyramid excercise from the lecture
import Prelude hiding (gcd)
pyramid :: String -> String
pyramid str = pyram 0 str where
    pyram :: Int -> String -> String
    pyram n str 
        | n < (length str + 1) `div` 2 = spaces ++ substr ++ newline ++ pyram (n+1) str
        | otherwise                    = ""
        where
            spaces  = replicate n ' '
            substr  = reverse (drop n (reverse (drop n (str))))
            newline = "\n"

main :: IO ()
main = putStr (pyramid "Functional programming is fun")


-- Exercise 1.2
bigger :: Integer -> Integer -> Integer 
bigger m n
    | m < n     = m
    | otherwise = n

copy :: String -> Integer -> String 
copy s n
    | n <= 0    = ""
    | otherwise = s ++ copy s (n-1)

gcd :: Integer -> Integer -> Integer 
gcd x 0 = x
gcd x y = gcd y (x `mod` y)


-- Exercise 1.3
-- Evaluates to 42
e1 = 1 + 125 * 8 `div` 10 - 59
-- Evaluates to False
e2 = not True || True && False
-- Evaluates to True
e3 = 1 + 2 == 6 - 3
-- Evaluates to False
e4 = "1 + 2" == "6 - 3"
-- Evaluates to True
e5 = "1111 + 2222" == "1111" ++ " + " ++ "2222"


-- Exercise 1.4
-- Normal-order evaluation: expanding the expression first (call by need, lazy)
{-
  double 5
=   { definition of double }
  twice (\y -> 5 + y) 0 
=   { definition of twice }
  (\y -> 5 + y) ((\y -> 5 + y) 0)
=   { evaluation of inner lambda }
  (\y -> 5 + y) (5 + 0)
=   { definition of (+) }
  (\y -> 5 + y) 5
=   { evaluation of lambda }
  5 + 5
=   { definition of (+) }
  10
-}

-- Exercise 1.7
-- Part 1
triangle :: Integer -> String
triangle n = drawTriangle (fromIntegral 1) (fromIntegral (n-1)) 

drawTriangle :: Int -> Int -> String
drawTriangle st 0  = replicate st '*'
drawTriangle st sp = replicate sp ' ' ++ replicate st '*' ++ "\n" ++ drawTriangle (st+2) (sp-1)

-- Part 2
drawTriangleShift :: Int -> Int -> Int -> String
drawTriangleShift 0  st _ = drawTriangle st 0
drawTriangleShift sh st 0  = replicate sh ' ' ++ replicate st '*'
drawTriangleShift sh st sp = replicate sh ' ' ++ replicate sp ' ' ++ replicate st '*' ++ "\n" ++ drawTriangleShift sh (st+2) (sp-1)
-- sh = shift, st = stars, sp = spaces

christmasTree :: Integer -> String
christmasTree n = drawChristmasTree (fromIntegral n) 0

drawChristmasTree :: Int -> Int -> String
drawChristmasTree 0  _ = ""
drawChristmasTree sh ts = drawTriangleShift sh 1 ts ++ "\n" ++ drawChristmasTree (sh-1) (ts+1)
-- ts = triangle size
