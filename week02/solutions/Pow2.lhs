> module Pow2 where

2.3.1:

Remark: we could have actually also done this exercise with just "Eq n" instead of "Ord n", if we don't mind
that the function simply loops forever if we ask for, say "pow2 -1". But it is slightly easier to program with
"Ord" (which allows checking for inequality) than only "Eq".

> pow2 :: (Ord n, Num n, Num a) => n -> a
> pow2 n 
>   | n < 0 = error "illegal input to pow2"
> pow2 0 = 1
> pow2 n = 2*pow2 (n-1)

Here's an example of a pow2 definition that does not work for this exercise; it requires the "Integral"
class to be satisfied by the first argument:

> pow2' :: Integer n -> Integer
> pow2' 0 = 1
> pow2' n = sqr (pow2' (n `div` 2)) * if odd n then 2 else 1
>    where sqr x = x*x

For the same reason, pow2 = 2^n, which is an excellent definition otherwise, would not have worked for this exercise.

2.3.2:

* Integer: there is on maximum n, since Integer is an arbitrary-precision data type

* Int: the maximum n is 62; on my machine, I run the 64-bits GHC, so Int is a signed 64-bits integer, which means that
  the maximum value it can hold is 2^63-1 (2^63 overflows to a negative number, 2^64 will come out as zero)

* Float: the maximum n is 127, consistent with this being a 32-bits single-precision floating point format;
  higher n's produce Infinity; the maximum value Float can hold is (2^24 - 1) * 2^(127 - 23).

* Double: here the maximum is 1023, consistent with 64-bits double-precision
  the maximum value Double can hold is (2^53 - 1) * 2^(1023 - 52).
