module FindDefs where

(?$) :: Maybe (a -> b) -> Maybe a -> Maybe b
(?$) Nothing _ = Nothing
(?$) _ Nothing = Nothing
(?$) (Just f) (Just a) = Just (f a)

-- product :: (Applicative t) => t a -> t b -> t (a,b)

apply :: [a -> b] -> a -> [b]
apply (f:fs) x = f x : apply fs x

-- apply2nd :: [a -> b -> c] -> b -> [a -> c]
