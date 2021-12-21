module Demo where

import qualified Data.Map as Map

expr1,expr2 :: Maybe Bool
expr3 :: Maybe a
expr4 :: Maybe Int

expr1 = Just True
-- foldr (+) 0 expr1      = type error: (+) can only be used on Nums
-- foldr (&&) True expr1  = True
-- foldr (||) False expr1 = True

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr1                    = type error
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr1 = Just <replacement>
-- traverse print expr1                                                         = True
--                                                                                Just ()   

expr2 = Just False
-- foldr (+) 0 expr2      = type error
-- foldr (&&) True expr2  = False
-- foldr (||) False expr2 = False

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr2                    = type error
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr2 = Just <replacement>
-- traverse print expr2                                                         = False
--                                                                                Just ()

expr3 = Nothing
-- foldr (+) 0 expr3      = 0
-- foldr (&&) True expr3  = True
-- foldr (||) False expr3 = False

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr3                    = Just Nothing
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr3 = Nothing
-- traverse print expr3                                                         = Nothing

expr4 = Just 42
-- foldr (+) 0 expr4      = 42
-- foldr (&&) True expr4  = type error
-- foldr (||) False expr4 = type error

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr4                    = Nothing
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr4 = Just <replacement>
-- traverse print expr4                                                         = 42
--                                                                                Just ()

expr5 :: [a]
expr6 :: [Int]
expr7 :: [Bool]

expr5 = []
-- foldr (+) 0 expr5      = 0
-- foldr (&&) True expr5  = True
-- foldr (||) False expr5 = False

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr5                    = Just []
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr5 = []
-- traverse print expr5                                                         = []

expr6 = [1..5]
-- foldr (+) 0 expr6      = 15
-- foldr (&&) True expr6  = type error
-- foldr (||) False expr6 = type error

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr6                    = Just [1,2,3,4,5]
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr6 = [<r1>,<r2>,<r3>,<r4>,<r5>]
-- traverse print expr6                                                         = 1
--                                                                                2
--                                                                                3
--                                                                                4
--                                                                                5
--                                                                                [(),(),(),(),()]

expr7 = [True,True,False]
-- foldr (+) 0 expr7      = type error
-- foldr (&&) True expr7  = False
-- foldr (||) False expr7 = True

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr7                    = type error
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr7 = [<r1>,<r2>,<r3>]
-- traverse print expr7                                                         = True
--                                                                                True
--                                                                                False
--                                                                                [(),(),()]

expr8, expr9 :: Map.Map String Double
expr8 = Map.empty
-- foldr (+) 0 expr8      = 0.0
-- foldr (&&) True expr8  = type error
-- foldr (||) False expr8 = type error 

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr8                    = Just (fromList [])
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr8 = fromList []
-- traverse print expr8                                                         = fromList []

expr9 = Map.fromList [("Rinus", 7.5), ("Peter", 8.2), ("Ralf", 6.8)]
-- foldr (+) 0 expr9      = 22.5
-- foldr (&&) True expr9  = type error
-- foldr (||) False expr9 = type error

-- traverse (\x -> if x >= 6 then Nothing else Just x) expr9                    = Nothing
-- traverse (\x -> putStr "enter replacement for " >> print x >> getLine) expr9 = fromList [("Peter",<r1>),("Ralf",<r2>),("Rinus",<r3>)]
-- traverse print expr9                                                         = 8.2
--                                                                                6.8
--                                                                                7.5
--                                                                                fromList [("Peter",()),("Ralf",()),("Rinus",())]
