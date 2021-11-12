module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

-- 8.7.1:
{-
We start by deriving the definition using induction and the property:
inorderCat t xs = inorder t ++ xs

Case 1: t = Leaf

  inorderCat Leaf xs
= { specification of inorderCat }
  inorder Leaf ++ []
= { definition of inorder }
  [] ++ []
= { definition of (++) }
  [] 
  
Case 2: t = (Node x t1 t2)
IH1: inorderCat t1 xs = inorder t1 ++ xs
IH2: inorderCat t2 xs = inorder t2 ++ xs

  inorderCat (Node x t1 t2) xs
= { specification of inorderCat }
  inorder (Node x t1 t2) ++ xs
= { definition of inorder }
  inorder t1 ++ [x] ++ inorder t2 ++ xs
= { IH1 and IH2 }
  inorderCat t1 [x] ++ inorderCat t2 xs

Thus, we have the following definition:
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf _            = []
inorderCat (Node x t1 t2) xs = inorderCat t1 [x] ++ inorderCat t2 xs

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

