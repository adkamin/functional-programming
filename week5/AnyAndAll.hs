module AnyAndAll where

all' :: (a -> Bool) -> [a] -> Bool
all' p = not . any (not . p)