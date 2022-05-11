import Data.Char
import Data.List

type Person = (String, Int, Char, String)

getName :: Person -> String
getName (x,_,_,_) = x

getAge :: Person -> Int
getAge (_,x,_,_) = x

getSex :: Person -> Char
getSex (_,_,x,_) = x

getResidence :: Person -> String
getResidence (_,_,_,x) = x

person1 :: Person
person1 = ("Name1", 1, 'M', "Streetname1")
person2 :: Person
person2 = ("Name2", 2, 'F', "Streetname2")
person3 :: Person
person3 = ("Name3", 31, 'M', "Streetname3")
person4 :: Person
person4 = ("Name4", 32, 'F', "Streetname4")
person5 :: Person
person5 = ("Name5", 35, 'F', "Streetname5")

database :: [Person]
database = [person1, person2, person3, person4, person5]

incRec :: [Person] -> Int -> [Person]
incRec [] _ = []
incRec (x:xs) n = (getName x, (getAge x) + n, getSex x, getResidence x) : incRec xs n

incComp :: [Person] -> Int -> [Person]
incComp xs n = [(a, b + n, c, d) | (a, b, c, d) <- xs]

incMap :: [Person] -> Int -> [Person]
incMap xs n = map (\(a, b, c, d) -> (a, b + n, c, d)) xs

namesRec :: [Person] -> [String]
namesRec [] = []
namesRec (x:xs)
    | getSex x == 'F' && getAge x >= 30 && getAge x <= 40 = getName x : namesRec xs
    | otherwise = namesRec xs

namesComp :: [Person] -> [String]
namesComp xs = [a | (a, b, c, d) <- xs, c == 'F', b >= 30, b <= 40]

namesFilter :: [Person] -> [String]
namesFilter xs = map (getName) (filter (\(a, b, c, d) -> if (c == 'F' && b >= 30 && b <= 40) then True else False) xs)

ageByName :: [Person] -> String -> Int
ageByName (x:xs) name
    | map toLower (getName x) == map toLower name = getAge x
    | otherwise = ageByName xs name

sortByAge :: [Person] -> [Person]
sortByAge xs = [(b, a, c, d) | (a, b, c, d) <- (sort [(b, a, c, d) | (a, b, c, d) <- xs])]