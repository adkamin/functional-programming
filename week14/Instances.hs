{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Instances where

import Result

-- you should only implement *either* foldr or foldMap; doing both is unnecessary
instance Foldable Result where
  -- foldr :: (a -> b -> b) -> b -> Result a -> b
  -- foldr f acc (Error s) = acc
  -- foldr f acc (Okay y)  = f y (foldr f acc (Okay y)) --will this terminate? looks like infinite recursion

  --foldMap  :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap f (Error s) = mempty
  foldMap f (Okay y)  = f y <> mempty

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error msg) = pure (Error msg)
  traverse f (Okay x)    = Okay <$> f x

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq,Show)

instance Foldable Tree where
  --foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap _ Leaf         = mempty
  foldMap f (Node n l r) = f n <> foldMap f l <> foldMap f r <> mempty

instance Functor Tree where
  --fmap :: (a -> b) -> f a -> f b
  fmap _ Leaf         = Leaf
  fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)

instance Traversable Tree where
  --traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  traverse f (Node n l r) = pure (Node) <*> f n <*> traverse f l <*> traverse f r

assistants :: Tree String
assistants = Node "Patrick" 
               (Node "Jen" 
                  (Node "Cassian" (Node "Bram" Leaf Leaf) Leaf) 
                  (Node "Mario" Leaf Leaf))
               (Node "Sander" 
                  (Node "Rico" (Node "Quinten" Leaf Leaf) Leaf)
                  (Node "Willem" Leaf Leaf))

flatten :: (Foldable t) => t String  -> String
flatten = foldr1 (\x y->x++", "++y)
