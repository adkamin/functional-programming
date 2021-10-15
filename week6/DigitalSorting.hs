module DigitalSorting where

import Data.List
import Data.Bool
import Data.Maybe
import Data.Either
import Data.Function

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

--1
genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd) . groupBy (on (==)fst) . sortOn fst

--2
instance Rankable Int where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

--3
instance Rankable Bool where
  rank bs = [[v | (k,v) <- bs, k],[v | (k,v) <- bs, not k]]
-- To do this in one pass, we can use foldr with a helper function which appends the element to the relevant list based on its value

--4
-- rank :: (Rankable key1, Rankable key2) ⇒ [((key1,key2),a)] → [[a]]
-- assoc :: ((k1,k2),a) → (k1,(k2,a))

-- instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
--   rank ts = 

