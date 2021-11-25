module Database where 

type Person = (Name, Age, FavouriteCourse) 

type Name             = String
type Age              = Integer 
type FavouriteCourse  = String 

elena, peter, pol :: Person 
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming") 
pol    =  ("Pol",    36,  "Object Oriented Programming")
sjaak  =  ("Sjaak",  26,  "Software Verification") 
frits  =  ("Frits",  61,  "Functional Programming") 
twan   =  ("Twan",   21,  "Category Theory")

students :: [Person] 
students = [elena, peter, pol, sjaak, frits, twan]

age :: Person -> Age 
age (_, n, _)  =  n

name:: Person -> Name 
name (s, _, _) = s

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, s) = s 

-- note: the local binding for 'name' and 'age' will override the global one 
-- this is valid Haskell, but ghci -Wall will produce a warning about it as 
-- certain forms of name shadowing can lead to difficult-to-detect problems
showPerson :: Person -> String
showPerson (name, age, fav)
  = "Name: " ++ name 
    ++ "nAge: " ++ show age
    ++ "nFavouriteCourse: " ++ show fav

twins :: Person -> Person -> Bool
twins (_, age1, _) (_, age2, _) = age1 == age2 

increaseAge :: Person -> Person
increaseAge (name', age', fav) = (name', age'+1, fav)

-- increment the age of all students by two 
query1   = map increaseAge (map increaseAge students)

-- promote all students
promote :: Person->Person
promote (name,age,fav) = ("dr. "++name, age, fav) 

query2   = map promote students 

-- find all students named Frits; as above: 
query3   = let isFrits p = name p == "Frits" in filter isFrits students 

-- find all students that are in their twenties
query4   = let inTwentiеs p = 20 <= age p && age p < 30 
           in filter inTwentiеs students 

-- using a where instead of a let; this is purely a matter of taste 
query4'  = filter inTwеnties students 
  where inTwеnties p = 20 <= age p && age p < 30 

-- compute the average age of all students; this runs into lovely typing problems! 
query5   = sum (map age students) `div` toInteger (length students)
query5'  = fromIntegral (sum (map age students)) / fromIntegral(length students)

-- promote all students whose favourite course is FP; 
query6   = let promoteFp p = if favouriteCourse p == "Functional Programming" then promote p else p
           in map promoteFp students 

-- solution2: split the students in two groups; promote one group and combine them 
query6'  = let likes p    = favouriteCourse p == "Functional Programming" 
               dislikes p = favouriteCourse p /= "Functional Programming"
           in map promote (filter likes students) ++ filter dislikes students
