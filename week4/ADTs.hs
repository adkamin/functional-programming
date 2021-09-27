module ADTs where

data Day       = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

d1, d2, d3 :: Day
d1 = Mon
d2 = Tue
d3 = Wed

data Prop      = Prop :-> Prop | T | F
  deriving Show

p1, p2, p3 :: Prop
p1 = T
p2 = F
p3 = T :-> F

data Unit      = Unit
  deriving Show

u1 :: Unit
u2 :: a -> Unit
u3 :: Unit -> Unit
u1 = Unit
u2 = \u -> Unit
u3 = id

data Foo a     = Bar a
  deriving Show

f1, f2, f3 :: Foo Int
f1 = Bar 1
f2 = Bar 2
f3 = Bar 3

data Tuple a b = Two a b | One a | None
  deriving Show

t1, t2, t3 :: Tuple Int Int
t1 = Two 1 2
t2 = One 1
t3 = None

data Wrapped a = Wrapped (Wrapped a) | Bare a
  deriving Show

w1, w2, w3 :: Wrapped Char
w1 = (Bare 'a')
w2 = Wrapped (Bare 'b')
w3 = Wrapped (Wrapped (Bare 'c')) 