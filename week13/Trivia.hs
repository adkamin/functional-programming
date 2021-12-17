module Trivia where

import Control.Applicative
import Parser

dot :: Parser Char
dot = char '.'

----------- 13.2.1 ------------

p1 = parse dot ""       -- evaluates to Nothing
-- This is because there is no '.' at the first index, we return Nothing.
-- The remainder of the string is not important.

p2 = parse dot "."      -- evaluates to Just ('.',"")
p3 = parse dot ".."     -- evaluates to Just ('.',".")
p4 = parse dot "...etc" -- evaluates to Just ('.',"..etc")
-- This is because there is '.' at the first index, so we return Just tuple
-- where first element is '.' and the second element is the remaining string to parse.


----------- 13.2.2 ------------

p1' = parse (many dot) "..b.c"  -- evalueates to Just ("","")
-- This is because many returns empty string if there is no '.' at the first index

p2' = parse (many dot) "."      -- evalueate to Just (".","")
p3' = parse (many dot) ".."     -- evaluates to Just ("..","")
p4' = parse (many dot) "...etc" -- evaluates to Just ("...","etc")
-- This is because if '.' is at the first index, 
-- it allows to parse more '.' characters till no more found


----------- 13.2.3 ------------

-- Running many (many dot) as a parser enters an infinite loop because 


-- loop :: Parser Char
-- loop = many (many char)

-- dots :: Parser String
-- dots = (++) <$> many dot <*> many dot
