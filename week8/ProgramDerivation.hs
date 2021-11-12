module Expression where

import Data.Time ( diffUTCTime, getCurrentTime )

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
  inorder Leaf ++ xs
= { definition of inorder }
  [] ++ xs
= { definition of (++) }
  xs
  
Case 2: t = (Node x t1 t2)
IH1: inorderCat t1 xs = inorder t1 ++ xs
IH2: inorderCat t2 xs = inorder t2 ++ xs

  inorderCat (Node x t1 t2) xs
= { specification of inorderCat }
  inorder (Node x t1 t2) ++ xs
= { definition of inorder }
  inorder t1 ++ [x] ++ inorder t2 ++ xs
= { definition of (++) }
  inorder t1 ++ x : inorder t2 ++ xs
= { IH2 }
  inorder t1 ++ (x : inorderCat t2 xs)
= { IH1 }
  inorderCat t1 (x : inorderCat t2 xs)
  

Thus, we have the following definition:
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs           = xs
inorderCat (Node x t1 t2) xs = inorderCat t1 (x : inorderCat t2 xs)

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- 8.7.2
{-
After testing the run time of both using the test function, it turns out that for
depth 10000, inorder' took ~0.4s and inorder took ~3.7s meaning inorder' is actually
more efficient.
-}

test :: IO ()
test = do start <- getCurrentTime
          print $ inorder' (skewed 10000)
          stop <- getCurrentTime
          print $ diffUTCTime stop start
          start1 <- getCurrentTime
          print $ inorder (skewed 10000)
          stop1 <- getCurrentTime
          print $ diffUTCTime stop1 start1


-- 8.7.3
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

{-
We assume elemsCat t xs = elems t ++ xs

Case 1: t = Leaf

  elemsCat Leaf xs
= { above specification }
  elems Leaf ++ xs
= { definition of elems }
  [] ++ xs
= { definition of (++) }
  xs

Case 2: t = (Node x t1 t2)
IH1: elemsCat t1 xs = elems t1 ++ xs
IH2: elemsCat t2 xs = elems t2 ++ xs

  elemsCat (Node x t1 t2) xs
= { above specification }
  elems (Node x t1 t2) ++ xs
= { definition of elems }
  x : elems t1 ++ elems t2 ++ xs
= { IH1 }
  x : elemsCat t1 (elems t2 ++ xs)
= { IH2 }
  x : elemsCat t1 (elemsCat t2 xs)

Thus, we have the following definition:
-}

elemsCat :: Tree a -> [a] -> [a]
elemsCat Leaf xs           = xs
elemsCat (Node x t1 t2) xs = x : elemsCat t1 (elemsCat t2 xs)

elems' :: Tree a -> [a]
elems' t = elemsCat t []

{-
Again, elems' took ~0.4s while elems took ~3.9s, showing that the efficiency of elems' is better
and the improvement is the same as in inorder'.
-}

test2 :: IO ()
test2 = do start <- getCurrentTime
           print $ elems' (skewed 10000)
           stop <- getCurrentTime
           print $ diffUTCTime stop start
           start1 <- getCurrentTime
           print $ elems (skewed 10000)
           stop1 <- getCurrentTime
           print $ diffUTCTime stop1 start1