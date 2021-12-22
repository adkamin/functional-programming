{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

import Control.Monad.State
import Data.List
import Data.Maybe

data Expr var = Var var | Lit Integer | Op BinOp (Expr var) (Expr var)
  deriving (Show,Eq)
data BinOp    = Add | Sub | Mul | Div
  deriving (Show,Eq)

instance Functor Expr where
  --fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Lit i)    = Lit i
  fmap f (Var v)    = Var (f v)
  fmap f (Op o x y) = Op o (fmap f x) (fmap f y)

instance Foldable Expr where
  -- foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Lit i)    = mempty
  foldMap f (Var v)    = f v
  foldMap f (Op o x y) = foldMap f x <> foldMap f y

instance Traversable Expr where
  --traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Lit i)    = pure (Lit i)
  traverse f (Var v)    = fmap Var (f v)
  traverse f (Op o x y) = pure (Op o) <*> traverse f x <*> traverse f y

allVars :: (Ord a) => Expr a -> [a]
allVars = nub . foldr (:) []

renameVar :: String -> State [(String,Int)] Int
renameVar name = do
  (assoc) <- get
  case lookup name assoc of 
    (Just i) -> return i
    Nothing  -> do 
      let j = (length assoc)
      put ((name,j) : assoc)
      return j

renameAllVars :: Expr String -> State [(String,Int)] (Expr Int) 
renameAllVars expr = traverse renameVar expr

indexVars :: Expr String -> Expr Int
indexVars expr = do
  evalState (renameAllVars expr) []

