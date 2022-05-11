allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [a] = True
allEqual (x:xs:xss) = x == xs && allEqual (xs:xss)

isAS :: (Num a, Eq a) => [a] -> Bool
isAS [] = False
isAS [a] = False
isAS xs = allEqual $ zipWith (-) (drop 1 xs) xs