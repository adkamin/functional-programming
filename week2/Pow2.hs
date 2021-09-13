module Pow2 where

import Numeric

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

-- (first bit is sign, therefore instead of 32 and 64, there is a space for 31 and 63 bits to represent numbers
-- Int has an upper bound of 2^63
-- Integer is unlimited, so this depends on a specific hardware