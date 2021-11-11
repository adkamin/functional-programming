> module TreeInduction where
> 
> data Tree a = Leaf | Node a (Tree a) (Tree a)
>   deriving (Show)

1: What is the induction scheme for trees?
Base case: Show that property holds for Leaf 
    P(Leaf)

Inductive step: Assume that property holds for some Tree y and Tree z, for all y,z
and show that it holds for Node x
    P(Tree y) and P(Tree z) -> for all x, P(Node x (Tree y) (Tree x))

Local definitions:

> leaves :: Tree a -> Int
> leaves Leaf = 1                            --(1)
> leaves (Node _ l r) = leaves l + leaves r  --(2)
>
> nodes :: Tree a -> Int
> nodes Leaf = 0                             --(3)
> nodes (Node _ l r) = 1 + nodes l + nodes r --(4)

2: To prove: leaves t = nodes t + 1
By induction on t.

Case 1: t = Leaf

  leaves Leaf
= { 3 }
  1
= { identity of (+) }
  0 + 1
= { 3, right-to-left }
  nodes Leaf + 1

Case 2: t = Node x t1 t2
IH1: leaves t1 = nodes t1 + 1
IH2: leaves t2 = nodes t2 + 1

  leaves (Node x (Tree t1) (Tree t2))
= { 2 }
  leaves t1 + leaves t2
= { IH1 and IH2 }
  nodes t1 + 1 + nodes t2 + 1
= { 4, right-to-left }
  nodes (Node x (Tree t1) (Tree t2)) + 1