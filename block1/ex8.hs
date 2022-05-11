mylength :: [a] -> Int
mylength []     = 0
mylength (x:xs) = 1 + mylength(xs)

mysum :: (Num a) => [a] -> a
mysum []      = 0
mysum (x:xs)  = x + mysum(xs)

myreverse :: [a] -> [a]
myreverse []      = []
myreverse (x:xs)  = myreverse xs ++ [x]

mytake :: [a] -> Int -> [a]
mytake (x:xs) n
  | n > mylength (x:xs) = (x:xs)
  | n <= 0              = []
  | otherwise           = [x] ++ mytake (xs) (n-1)
mytake [] n = []

myelem :: Eq a => [a] -> a -> Bool
myelem (x:xs) elem
  | x == elem = True
  | otherwise = myelem (xs) elem
myelem [] elem = False

myconcat :: [[a]] -> [a]
myconcat [xs] = xs
myconcat (xs:xss) = xs ++ (myconcat xss)

mymaximum :: Ord a => [a] -> a
mymaximum [] = error "empty list"
mymaximum [x] = x
mymaximum (x:xs) = max x (mymaximum xs)

myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys


