module Pow2 where

import Numeric

-- Part 1 & 2
pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

-- Part 3
{-
(The first bit is sign.)
Int has an upper bound of 2^63.
Integer is unlimited, so this depends on the hardware.
Float is single precision, so the upper bound is 2^31
Double is double precision, so the upper bound is 2^63
-}