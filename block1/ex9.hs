r :: (Num a, Enum a) => a -> a -> [a]
r a d = [ x | x <- [a,a+d..] ]

r1 :: (Num a, Enum a) => a -> a -> Int -> a
r1 a d n = xs !! n
  where xs = r a d

totalr :: (Num a, Enum a ) => a -> a -> Int -> Int -> a
totalr a d i j = sum $ take (j-i) $ drop i $ xs
  where xs = r a d