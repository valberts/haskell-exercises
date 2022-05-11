allRowsEquallyLong :: [[a]] -> Bool
allRowsEquallyLong (x:[]) = True
allRowsEquallyLong (x:xs:xss) = length x == length xs && allRowsEquallyLong (xs:xss)

rowTotals :: Num a => [[a]] -> [a]
rowTotals xss = map sum xss

mytranspose :: [[a]] -> [[a]]
mytranspose ([]:_) = []
mytranspose xss = (map head xss) : mytranspose (map tail xss)

colTotals :: Num a => [[a]] -> [a]
colTotals xss = rowTotals (mytranspose xss)