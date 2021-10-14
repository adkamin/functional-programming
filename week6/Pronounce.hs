module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'" ++ [c] ++ "'"]

instance Pronounceable Int where
  pronounce = say . toInteger

instance Pronounceable Double where
  pronounce d = say (toInteger whole) ++ " point " ++ say (floor (fractional * 10))
    where 
      (whole, fractional) = properFraction d

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)

instance (Pronounceable a, Pronounceable b) => Pronounceable (a,b) where
  pronounce (x,y) = "a tuple: (" ++ pronounce x ++ ", " ++ pronounce y ++ ")"