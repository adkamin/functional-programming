module Database where

data Person = Person { name::String, age::Integer, favouriteCourse::String }

elena, peter, pol, andrea :: Person

elena  = Person {name="Elena",  age=33, favouriteCourse="Functional Programming"}
peter  = Person {name="Peter",  age=57, favouriteCourse="Imperative Programming"}
pol    = Person {name="Pol",    age=36, favouriteCourse="Object Oriented Programming"}
andrea = Person {name="Andrea", age=23, favouriteCourse="Software Engineering"}

students :: [Person]
students = [elena, peter, pol, andrea]

showPerson :: Person -> String
showPerson (Person name age favouriteCourse) = "Name: " ++ name ++ ", Age: " ++ show age ++ ", Favourite course: " ++ favouriteCourse

twins :: Person -> Person -> Bool
twins p1 p2 = age p1 == age p2

increaseAge :: Person -> Person
increaseAge p = p {age = age p + 1}

increaseAll :: [Person] -> [Person]
increaseAll = map (\p -> p {age = age p + 1})

promoteAll :: [Person] -> [Person]
promoteAll = map (\p -> p {name = "dr. " ++ name p} )

findFrits :: [Person] -> [Person]
findFrits ps = filter (\p -> name p == "Frits") ps

findTwenties :: [Person] -> [Person]
findTwenties = filter (\p -> age p >= 20 && age p < 30)

getAvgAge :: [Person] -> Integer
getAvgAge lst = (sum (map (\p -> age p) lst)) `div` toInteger (length lst)

promoteFP :: [Person] -> [Person]
promoteFP lst = map (\p -> p {name = "dr " ++ name p}) (filter (\p -> favouriteCourse p == "Functional Programming") lst)