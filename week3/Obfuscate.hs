module Obfuscate where

import Data.Char

-- divide string into words, shuffle, and convert back to string
cambridge :: String -> String
cambridge = unwords' . shuffleAll . words'

-- Uses space as seprator, treats other punctuation and nummbers like words
words' :: String -> [String] 
words' []  = []
words' l@(x:xs) 
    | x == ' '             = words' xs
    | x == '.' || x == ',' = [x] : words' xs
    | otherwise            = before : words' after
                                where (before, after) = break (== ' ') l

-- Puts list back to a string, if last element was punctuation, space is ommitted
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (x:y:xs)
    | y == "."  || y == "," = x ++ y ++ unwords' xs
    | otherwise             = x ++ " " ++ y ++ " " ++ unwords' xs

-- shuffle each word
shuffleAll :: [String] -> [String]
shuffleAll = map shuffleWord 

-- if the word has more than 2 characters, shuffle the word leaving out the first and last characters 
-- otherwise keep the word as it is
shuffleWord :: String -> String
shuffleWord l@(s:ss)
    | length l < 3 = l
    | otherwise = s : (shuffle (take ((length ss)-1) ss)) ++ [last ss]

-- shuffles the relevant part by dividing the string into halves and interchangeably taking first element of each list
shuffle :: String -> String
shuffle s = interchange (take ((length s) `div` 2) s) (drop ((length s) `div` 2) s)

-- deterministic shuffling: interchanges elements from two lists
-- taking from the second list first, so that words like "right" would shuffle
interchange :: [a] -> [a] -> [a]
interchange []  []         = []
interchange [x] []         = [x]
interchange [] [y]         = [y]
interchange (x:xs) (y:ys)  = y : x : interchange xs ys

-- test string
meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."
