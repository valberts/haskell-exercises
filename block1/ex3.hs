import Test.QuickCheck

prop_commadd :: Int -> Int -> Bool
prop_commadd a b = (a + b) == (b + a)
prop_commsub :: Int -> Int -> Bool
prop_commsub a b = (a - b) == (b - a)