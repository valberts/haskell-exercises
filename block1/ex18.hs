import Data.List
import Test.QuickCheck

bsort :: (Ord a) => [a] -> [a]
bsort [] = []
bsort x = case nextIter == x of 
        True -> x
        False -> bsort nextIter
    where nextIter = bsortinner x

bsortinner [] = []
bsortinner [x] = [x]
bsortinner (x:y:xs)
    | y <= x = y : bsortinner (x:xs)
    | otherwise = x : bsortinner (y:xs)

mmsort :: (Ord a) => [a] -> [a]
mmsort [] = []
mmsort [x] = [x]
mmsort xs = [minimum xs]  ++ mmsort (xs \\ (minimum xs : maximum xs : [])) ++ [maximum xs]

ins' :: (Ord a) => a -> [a] -> [a]
ins' x [] = [x]
ins' x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y : (ins' x ys)

isort :: (Ord a) => [a] -> [a]
isort xs = foldr (ins') [] xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
    | x <= y = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take len xs)) (msort (drop len xs))
    where len = (length xs) `div` 2

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let smaller = qsort [y | y <- xs, y <= x]
        bigger = qsort [y | y <- xs, y > x]
    in smaller ++ [x] ++ bigger

prop_bsort :: Ord a => [a] -> Bool
prop_bsort xs = bsort xs == sort xs