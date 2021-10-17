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
  rank bs = [[v | (k,v) <- bs, not k],[v | (k,v) <- bs, k]]

--4
instance (Rankable key1, Rankable key2) => Rankable (key1, key2) where
  rank = map (concat . rank) . rank . map assoc 
    where assoc ((k1,k2),a) = (k1,(k2,a))

--5
instance Rankable (Maybe key) where
  rank ms = [[v | (k,v) <- ms, isNothing k],[v | (k,v) <- ms, not (isNothing k)]]

--6
instance Rankable [key] where
  rank = rank . map (\(ks, v) -> (uncons ks, v))

--7
rankWithKey :: (Rankable key) => [(key,a)] -> [[(key,a)]]
rankWithKey = rank . map (\(k,v) -> (k, (k,v)))