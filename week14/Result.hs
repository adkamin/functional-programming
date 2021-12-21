module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  --fmap :: (a -> b) -> Result a -> Result b
  fmap f (Error s) = Error s
  fmap f (Okay a)  = Okay (f a)

instance Applicative Result where
  --pure :: a -> Result a
  pure = Okay

  --(<*>) :: f (a -> b) -> f a -> f b
  (Error f) <*> (Error a) = Error (f ++ a)
  (Error f) <*> _         = Error f
  _         <*> (Error a) = Error a
  (Okay f)  <*> (Okay a)  = Okay (f a)