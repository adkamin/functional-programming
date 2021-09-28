module Tree where

--import Data.List

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

{----------- exercise 4.3 -------------}

-- 1
t :: Tree Char
t = Node 'c' (Node 'a' Leaf (Node 'b' Leaf Leaf)) (Node 'f' (Node 'd' Leaf Leaf) (Node 'g' Leaf Leaf))

-- 2
{-
                  3
              /      \
            2          4
          /   \      /   \
         1     .    5     .
        / \        / \ 
       .   .      .   .  
-}

--3
leaves :: Tree a -> Int
leaves Leaf         = 1
leaves (Node a l r) = leaves l + leaves r

nodes  :: Tree a -> Int
nodes t = (leaves t) - 1

height :: Tree a -> Int
height Leaf = 0
height (Node a l r) = 1 + max (height l) (height r)

elems  :: Tree a -> [a]
elems Leaf         = []
elems (Node a l r) = elems l ++ [a] ++ elems r

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree t    = isSorted (elems t)

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = x <= y && isSorted (y:ys)

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member el Leaf = False
member el (Node a l r) 
  | el == a = True
  | el <  a = member el l
  | el >  a = member el r

insert :: (Ord a) => a -> Tree a -> Tree a
insert el Leaf = (Node el Leaf Leaf)
insert el (Node a l r) 
  | el == a = (Node a l r)
  | el <  a = (Node a (insert el l) r) 
  | el >  a = (Node a l (insert el r))


--delete :: (Ord a) => a -> Tree a -> Tree a

--fromList :: (Ord a) => [a] -> Tree a



{----------- exercise 4.5 -------------}

--inOrder :: Tree a -> [a]
--fromAscList :: [a] -> Tree a
--breadthFirst :: Tree a -> [a]

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree ++ "\n"
  where 
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  go pre _ Leaf = pre ++ "\n" -- change this to "" to get a more compact display
  go pre (preL,preR,preN) (Node k lt rt)
    = go (pre ++ preL) (hfill,v_bar,lbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++ 
      go (pre ++ preR) (v_bar,hfill,rbend) lt

  junct = "┤\n"
  hfill = fill ++ "  " 
  lbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  rbend = fill ++ "╰─"  -- change to "\-" if on Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)

