module Char where

import Data.Char

(~~) :: String -> String -> Bool
(~~) (x:xs) (y:ys) = toLower x == toLower y && (~~) xs ys
(~~) [] []         = True
(~~) [] _          = False
(~~) _ []          = False

reverseCase :: String -> String
reverseCase (x:xs) = if isLower x then toUpper x : reverseCase xs else toLower x : reverseCase xs
reverseCase []     = ""

shift :: Int -> Char -> Char
shift n c = chr ((ord c - 64 + n) `mod` 26 + 64)

caesar :: Int -> String -> String
caesar n (s:sx)
    | isAsciiLower s = shift n (toUpper s) : caesar n sx
    | isAsciiUpper s = shift n s : caesar n sx
    | otherwise              = s : caesar n sx
caesar n []     = ""

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
-- decoded to: "FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"

decode :: Int -> String -> String
decode 0 s = ""
decode n s = caesar n s ++ "\n" ++ decode (n-1) s