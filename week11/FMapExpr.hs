module FMapExpr where

import Data.Char

-- increments each value in the list
-- Functor: []
-- type of fmap: (a -> b) -> [] a -> [] b
expr1 = fmap (\x->x+1) [1,2,3]

-- prepends "dr." to "Sjaak" and wraps it inside Just
-- Functor: Maybe
-- type of fmap: (a -> b) -> Maybe a -> Maybe b
expr2 = fmap ("dr." ++) (Just "Sjaak")

-- converts string to all lower characters
-- Functor: toLower
-- type of fmap: (a -> b) -> [a] -> [b]
expr3 = fmap toLower "Marc Schoolderman"

-- adds "dr." to Justs in a list
-- Functor: Maybe []
-- type of inner fmap: (a -> b) -> Maybe a -> Maybe b
-- type of outer fmap: (a -> b) -> Maybe a -> Maybe b
expr4 = fmap (fmap ("dr." ++)) [Nothing, Just "Marc", Just "Twan"]
