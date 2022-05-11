import Test.QuickCheck

total1 :: Int -> Int
total1 0 = 0 
total1 n = total1 (n-1) + n

total2 :: Int -> Int
total2 n = (n * (n-1)) `div` 2 -- made n+1 to n-1 to make quick check fail

prop_total n = (n >= 0) ==> total1 n == total2 n