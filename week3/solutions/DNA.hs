module DNA where 

import Data.List

-- Nucleobases or DNA-bases are the basic building blocks of
-- deoxyribonucleic acid (DNA). 
-- Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

data Base  =  A | C | G | T
  deriving (Eq, Ord, Show) 

type DNA      =  [Base] 
type Segment  =  [Base]

exampleDNA :: DNA 
exampleDNA = [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

contains :: Segment -> DNA -> Bool
contains s dna = or [ s `isPrefixOf` dna' | dna' <- tails dna ]

containsI :: Segment -> DNA -> [Int]
containsI s dna = [ i | (i,dna') <- zip [0..] (tails dna), s `isPrefixOf` dna' ]

longestOnlyAs :: DNA -> Int
longestOnlyAs dna = maximum (0:[ length as | as@(A:_) <- group dna ]) -- note: prefix "0" prevents errors if there are no A 

{- General idea for this next list comprehension solution: mark all positions of A, calculate differences between the positions
 - of the first A and the 12th A (so there are 10 A's in between) the second A and the 13th A, and take the maximum.
 - Getting exactly the right idea, and getting it precisely right is not easy!  -} 

longestAtMostTenAs :: DNA -> Int 
longestAtMostTenAs dna = maximum lengths 
  where 
  lengths = [ a10 - a0 - 1 | (a0,a10) <- (replicate 11 (-1) ++ posA) `zip` (posA ++ replicate 11 (length dna)) ] 
  posA    = [ i | (i, A) <- zip [0..] dna ]

{- General idea for this alternative: generate a list of all the "in between" segments (between A's);
 - and group them in sections of at most 11 (so we have ten A's in between), and find the optimum! -}

longestAtMostTenAs' :: DNA -> Int 
longestAtMostTenAs' dna = maximum [ length seg11 + sum seg11 - 1 | seg <- tails (map length segs), let seg11 = take 11 seg ] 
  where
  segs = map (takeWhile (/=A)) (dna : [ seg | (A:seg) <- tails dna ]) -- a list of of all segments which reside between A's 

-- convert a string representation to DNA
toDNA :: String -> DNA
toDNA s = [ base | c <- s, (c',base) <- zip "ACGT" [A,C,G,T], c == c' ]

largerDNA :: DNA
largerDNA = toDNA 
  "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\ 
  \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
  \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\ 
  \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\ 
  \GACAATTTAATAT\ 
  \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
  \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
  \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
  \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

fileDNA :: (DNA -> a) -> FilePath -> IO a
fileDNA f fn = fmap (f.toDNA) (readFile fn)
infix 0 `fileDNA`
