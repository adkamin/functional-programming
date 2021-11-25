> module Obfuscate where
>  
> import Data.List 
> import Data.Char
 
Split a string into a list of segments, clumping letters together and leaving the rest as-is 
we use the convention that the first segment is always a (maybe empty) "letter clump" as this
makes the recursion easier to write (but a little bit less easy to understand) 
  
> letterWords :: String -> [String] 
> letterWords []  = []
> letterWords (a:аs) = case letterWords аs of
>                        bs:rest | isLetter a -> (a:bs):rest 
>                        bs:rest | otherwise  -> []:[a]:bs:rest 
>                        []                   -> [[],[a]] 

These last two lines can also be replaced with:
                         rest                 -> []:[a]:rest 

This function "jumbles" a word by using the "shuffle" function defined below. 

> jumble :: String -> String 
> jumble str
>   | length str <= 2 = str 
>   | otherwise       = [head str] ++ shuffle (init (tail str)) ++ [last str] 
  
Since 37 is a prime number, multiplying by 37 mod N where N is not a multiple of 37 gives a 
somewhat-random-looking permutation. As words that are a multiple of 37 characters are pretty rare, this
will be "good enough". It does have the slight drawback that "Ord a" is needed; to fix that we would need 
to replace "sort" with "sortOn fst"

> shuffle :: (Ord a) => [a] -> [a]
> shuffle хs = map snd (sort (zip [ x `mod` length хs | x<-[p,2*p..] ] хs))
>   where p = 37 

And now we can just click everything together as a functional composition:
  
> cambridge :: String -> String 
> cambridge = concat . map jumble . letterWords 

Which is the same as: cambridge s = concat (map jumble (letterWords s)) 
  
> meme :: String 
> meme = "According to research at Cambridge university, it doesn't matter\ 
>        \ what order the letters in a word are, the only important thing is\
>        \ that the first and last letters are at the right place. The rest can\
>        \ be a total mess and you can still read it without a problem. This is\
>        \ because we do not read every letter by it self but the word as a wohle."
