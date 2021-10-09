import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x -> (head x, length x)) . group . sort . words

mostFrequencyOfLength :: Int -> String -> [(String, Int)]
mostFrequencyOfLength n = reverse . (filter (\(s,i) -> length s >= n)) . wordFrequency 

wordLengthFrequency :: String -> [[(Int, Int)]]
wordLengthFrequency = sumLengths . groupLengths . getLengths
  where 
    sumLengths   = map collapse
    groupLengths = groupBy (\t1 t2 -> fst t1 == fst t2) . sortOn fst
    getLengths   = map (\(s,i) -> (length s, i)) . mostFrequencyOfLength 0 

-- don't know yet how to do this with foldr or map or something similar
collapse [(a,b)]            = [(a,b)]
collapse ((a,b):(c,d):rest) = collapse ((a,(b+d)):rest)
-- collapse = foldr (\((a,b):(c,d):rest) -> ((a,(b+d)):rest)) []

--missess removing duplicates
anagrams :: String -> [[String]]
anagrams = filter (\l -> length l > 1) . groupBy (\t1 t2 -> length (union t1 t2) == length t1) . reverse . (sortBy (compare `on` length)) . words

main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function
  where onStdin f = getContents >>= mapM_ print . f . filter (\x->isAlphaNum x || isSpace x)
