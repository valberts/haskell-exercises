sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

isPrime :: Int -> Bool
isPrime n = n `elem` (sieve [2..n])

nPrimes :: Int -> [Int]
nPrimes n = take n (sieve [2..])

nLessPrimes :: Int -> [Int]
nLessPrimes n = sieve [2..n]

dividers :: Int -> [Int]
dividers m = [x | x <- [1..m], m `mod` x == 0]

isPrime' :: Int -> Bool
isPrime' n = length (dividers n) == 2