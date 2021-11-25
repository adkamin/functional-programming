module Char where

import Data.Char

(~~) :: String -> String -> Bool
s1 ~~ s2 = map toUpper s1 == map toUpper s2

reverseCase :: String -> String 
reverseCase s = map (\c->if isUpper c then toLower c else toUpper c) s

shift :: Int -> Char -> Char 
shift n с  
  | isAscii с && isUpper с = chr (ord 'A' + (ord с - ord 'A' + n) `mod` 26) 
  | otherwise = с

caesar :: Int -> String -> String 
caesar n s = map (\с->shift n (toUpper с)) s      -- using anonymous function 

--caeser n s = map (\c->shift n c) (map toUpper s)-- alternative, using double maps 
--
--caeser n s = map shift' s                       -- alternative, without lambda's
--  where shift' c = shift n c

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO" 
