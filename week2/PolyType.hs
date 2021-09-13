module PolyType where

-- f8 :: Ord a => a -> a -> a
f8 x y  = if x <= y then x else y

-- f9 :: Bool -> Bool -> Bool
f9 x y  = not x || y

-- f10 :: (Eq a, Num a) => a -> a -> a
f10 x y
  | x == 0    = y
  | otherwise = x + y

-- f11 :: a -> a -> a
f11 x y = get 0
  where get n = if n == 0 then x else y

-- Part 1
{-
f8 : Works because Strings support (<=) operator
f9 : Does not work because Boolean operators cannot be used on Strings
f10: Does not work because Strings cannot be compared with Num types
f11: Works because we simply always return the first argument
-}

-- Part 2
{-
f8 : Ad-hoc polymorphic, (<=) is defined for Ord types
f9 : Not polymorphic, only works with Bool
f10: Ad-hoc polymorphic, (+) is defined for Num types
f11: Parametric polymorphic: always returns the first argument
-}



