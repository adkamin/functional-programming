module FindDefs where

import Data.Maybe
import Data.Monoid

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = map fromJust . map f

lift :: (a -> b -> Maybe c) -> (Maybe a -> Maybe b -> Maybe c)
lift f = g
    where g Nothing _         = Nothing
          g _ Nothing         = Nothing
          g (Just x) (Just y) = f x y

compute :: (Monoid n) => (a -> n) -> [a] -> n
compute f = mconcat . map f 

fuse :: (a -> b -> c) -> (a -> b) -> a -> c
fuse f g x = f x (g x)
