module Database where
import Data.List

data Person = Person { name::String, age::Integer, favouriteCourse::String }

elena, peter, pol :: Person 
elena  =  Person {name="Elena", age=33, favouriteCourse="Functional Programming"} 
peter  =  Person {name="Peter", age=57, favouriteCourse="Imperative Programming"} 
pol    =  Person {name="Pol",   age=36, favouriteCourse="Object Oriented Programming"} 
sjaak  =  Person {name="Sjaak", age=26, favouriteCourse="Software Verification"} 
frits  =  Person {name="Frits", age=61, favouriteCourse="Functional Programming"} 
twan   =  Person {name="Twan",  age=21, favouriteCourse="Category Theory"} 

students :: [Person] 
students = [elena, peter, pol, sjaak, frits, twan]

showPerson :: Person -> String
showPerson p
  = "Name: " ++ name p
    ++ "\nAge: " ++ show (age p) 
    ++ "\nFavouriteCourse: " ++ show (favouriteCourse p)

twins :: Person -> Person -> Bool
twins p1 p2 = age p1 == age p2 

increaseAge :: Person -> Person 
increaseAge p = p { age=age p+1 }

-- increment the age of all students by two
query1   = map increaseAge (map increaseAge students)

-- promote all students 
promote :: Person->Person
promote р = р { name="dr. "++ name р } 

query2   = map promote students 

-- find all students named Frits; as above: 
query3   = filter (\р->name р=="Frits") students 

-- find all students that are in their twenties 
query4   = filter (\р->20 <= age р && age р < 30) students

-- compute the average age of all students; this runs into lovely typing problems! 
query5   = sum (map age students) `div` toInteger (length students) 
query5'  = fromIntegral (sum (map age students)) / fromIntegral(length students)

-- promote all students whose favourite course is FP; 
query6   = map (\p->if favouriteCourse p == "Functional Programming" then promote p else p) students

-- solution2: split the students in two groups; promote one group and combine them 
query6'  = let (likes, dislikes) = partition (\p->favouriteCourse p == "Functional Programming") students 
           in map promote likes ++ dislikes
