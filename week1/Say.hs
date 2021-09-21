module Say where

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"

say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 14 = "fourteen"
say 15 = "fifteen"
say 16 = "sixteen"
say 17 = "seventeen"
say 18 = "eighteen"
say 19 = "nineteen"

say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"

say n  = removeSpace (sayThousands n)

sayDigit n               
    | n > 0     = say n
    | otherwise = ""

sayTens n    
    | n < 10    = sayDigit (n `mod` 10)
    | n < 20    = say n        
    | otherwise = say (n `mod` 100 - (n `mod` 10)) ++ " " ++ sayDigit (n `mod` 10)

sayHundreds n 
    | n < 100   = sayTens (n `mod` 100)
    | otherwise = say ((n `mod` 1000) `div` 100) ++ " hundred " ++ sayTens (n `mod` 100)

sayThousands n  
    | n < 1000  = sayHundreds (n `mod` 1000)
    | otherwise = say (n `div` 1000) ++ " thousand " ++ sayHundreds (n `mod` 1000)

removeSpace s
    | last s == ' ' = take ((length s) - 1) s
    | otherwise     = s