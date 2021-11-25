module Mingle where

(++/) :: [a] -> [a] -> [a]
[] ++/ ys     = ys
(x:xs) ++/ ys = x:(ys ++/ xs)

infixr 5 ++/

-- The function prepends elements interchangeably from each list

{-
  [1,2,3] ++/ [4,5]
=   { definition of ++/ }
  1:[4,5] ++/ [2,3]
=   { definition of ++/ }
  1:4:[2,3] ++/ [5]
=   { definition of ++/}
  1:4:2:[5] ++/ [3]
=   { definition of ++/ }
  1:4:2:5:[3] ++/ []
=   { definition of ++/ }
  1:4:2:5:3:[] ++/ []
=   { definition of ++/ }
  1:4:2:5:3
-}