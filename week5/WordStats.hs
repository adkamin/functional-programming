import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x -> (head x, length x)) . group . sort . words

mostFrequencyOfLength :: Int -> String -> [(String, Int)]
mostFrequencyOfLength n = reverse . (filter (\(s,i) -> length s >= n)) . wordFrequency 

wordLengthFrequency :: String -> [(Int, Int)]
wordLengthFrequency = map (\x -> (head x, length x)) . group . sort . map length . words

anagrams :: String -> [[String]]
anagrams = filterAnagrams . groupByEqualEl
  where 
    filterAnagrams = filter (\l -> not (all (== head l) l))
    groupByEqualEl = groupBy (\t1 t2 -> length (union t1 t2) == length t1) . reverse . (sortBy (compare `on` length)) . words

main :: IO ()
main = onStdin $ M.toList . \l -> M.fromListWith (+) (map (\s -> (s,1)) (words l))
  where onStdin f = getContents >>= mapM_ print . f . filter (\x->isAlphaNum x || isSpace x)