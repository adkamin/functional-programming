> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t
By induction on t

Case 1: t = Tip x

  map f (tips (Tip x))
= { definition of tips }
  map f [x]
= { definition of map (skipping the steps with appending []) }
  [f x]
= { definition of tips }
  tips (Tip f x)
= { definition of mapBtree }
  tips (mapBtree f (Tip x))

Case 2: t = Bin t1 t2
IH1: map f (tips t1) = tips (mapBtree f t1)
IH2: map f (tips t2) = tips (mapBtree f t2)

  map f (tips (Bin t1 t2))
= { definition of tips }
  map f (tips t1 ++ tips t2)
= { from property map f (as ++ bs) = (map f as) ++ (map f bs) }
  map f (tips t1) ++ map f (tips t2)
= { IH1 and IH2 }
  tips (mapBtree f t1) ++ tips (mapBtree f t2)
= { definition of tips }
  tips (Bin (mapBtree f t1) (mapBtree f t2))
= { definition of mapBtree }
  tips (mapBtree f (Bin t1 t2))