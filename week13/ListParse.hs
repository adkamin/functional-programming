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