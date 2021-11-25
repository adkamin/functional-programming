module Monoids where
import Data.Monoid


-- Boolean operator and 

newtype AndBool = AndB { fromAndB :: Bool }
    deriving (Show)

instance Semigroup AndBool where
    x <> y = AndB (fromAndB x && fromAndB y)

instance Monoid AndBool where
    mempty = AndB True    

{-
It is associative, which can be seen on examples below:
(T && T) && T = T && (T && T) because T && T = T
(T && F) && T = T && (F && T) because T && F = F

It has an identity element (true):
T && true = T
F && true = F
-}


-- Boolean operator or

newtype OrBool = OrB { fromOrB :: Bool }
    deriving (Show)

instance Semigroup OrBool where
    x <> y = OrB (fromOrB x || fromOrB y)

instance Monoid OrBool where
    mempty = OrB False

{-
It is associative, which can be seen on examples below:
(F || F) || F = F || (F || F) because F || F = F
(F || F) || T = F || (F || T) because F || T = T


It has an identity element (false):
T || false = T
F || false = F
-}


-- Boolean operator const F

newtype ConstFBool = ConstFB { fromConstFB :: Bool }
    deriving (Show)

instance Semigroup ConstFBool where
    x <> y = ConstFB False

instance Monoid ConstFBool where
    mempty = ConstFB False

{-
It is associative, since the arguments are ignored and a false is returned in any case.

It has an identity element, again because false is returned in any case
-}


-- Boolean operator XOR

newtype XORBool = XORB { fromXORB :: Bool }
    deriving (Show)

instance Semigroup XORBool where
    x <> y = XORB (fromXORB x /= fromXORB y)   -- XOR returns true iff elements are different

instance Monoid XORBool where
    mempty = XORB False

{-
It is associative, see below:
a b c | a XOR (b XOR c) = (a XOR b) XOR c
T T T | T   XOR   F     =     F   XOR   T  =  T      
T T F | T   XOR   T     =     F   XOR   F  =  F
T F T | T   XOR   T     =     T   XOR   T  =  F
F T T | F   XOR   F     =     T   XOR   T  =  F
F F T | F   XOR   T     =     F   XOR   T  =  T
F T F | F   XOR   T     =     T   XOR   F  =  T
T F F | T   XOR   F     =     T   XOR   F  =  T
F F F | F   XOR   F     =     F   XOR   F  =  F

It has an identity element, since any value XORed with False remains unchanged
-}