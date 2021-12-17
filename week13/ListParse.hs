{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 -}

intList :: Parser [Integer]
intList = symbol "{" *> (int `sepBy` space) <* symbol "}"

{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

intRecord :: Parser [Integer]
intRecord = do
    symbol "{"
    n <- integer
    symbol "#"
    list <- replicateM (fromInteger n) integer
    symbol "}"
    return list

-- replicateM :: (Monad m) => Int -> m a -> m [a]
-- replicateM 0 _  = return []
-- replicateM n mx = do
--     x  <- mx
--     xs <- replicateM (n-1) mx
--     return (x:xs)

-- parseN :: Int -> Parser a -> Parser [a]
-- parseN 0 [] = pure []
-- parseN 0 p  = Nothing 
-- parseN n p  = pure (:) <*> p <*> parseN (n-1) p <|> pure []

    
    -- symbol "{" *> (int `sepBy` space) <* symbol "}"