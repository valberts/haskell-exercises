myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs = [x | x <- xs, f x]

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f y [] = y
myfoldl f y (x:xs) = myfoldl f (y `f` x) xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f y [] = y
myfoldr f y (x:xs) = x `f` (myfoldr f y xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] _ = []
myzipWith f _ [] = []
myzipWith f (x:xs) (y:ys) = (x `f` y) : myzipWith f xs ys