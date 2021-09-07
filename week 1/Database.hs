module Database where

type Person = (Name, Age, FavouriteCourse)

type Name            = String
type Age             = Integer
type FavouriteCourse = String

elena, peter, pol, andrea :: Person
elena  = ("Elena",  33,  "Functional Programming")
peter  = ("Peter",  57,  "Imperative Programming")
pol    = ("Pol",    36,  "Object Oriented Programming")
andrea = ("Andrea", 23,  "Software Engineering")

students :: [Person]
students = [elena, peter, pol, andrea]

age :: Person -> Age
age (_, n, _) = n

name :: Person -> Name
name (s, _, _) = s

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, s) = s

showPerson :: Person -> String
showPerson p = "Name: " ++ name p ++ ", Age: " ++ show (age p) ++ ", Favourite course: " ++ favouriteCourse p

twins :: Person -> Person -> Bool
twins p1 p2 = age p1 == age p2

increaseAge :: Person -> Person
increaseAge (n, a, f) = (n, a+1, f)

increaseAll :: [Person] -> [Person]
increaseAll lst = map increaseAge (map increaseAge lst)

promoteAll :: [Person] -> [Person]
promoteAll = map (\p -> ("dr " ++ name p, age p, favouriteCourse p)) 

findFrits :: [Person] -> [Person]
findFrits = filter (\p -> name p == "Frits")

findTwenties :: [Person] -> [Person]
findTwenties = filter (\p -> age p >= 20 && age p < 30)

getAvgAge :: [Person] -> Integer
getAvgAge lst = (sum (map (\p -> age p) lst)) `div` toInteger (length lst)

promoteFP :: [Person] -> [Person]
promoteFP lst = map (\p -> ("dr " ++ name p, age p, favouriteCourse p)) (filter (\p -> favouriteCourse p == "Functional Programming") lst)