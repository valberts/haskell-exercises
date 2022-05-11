splitlist :: (Ord a) => [a] -> Int -> [[a]]
splitlist [] _ = []
splitlist (x:xs) n = (take n (x:xs)) : splitlist xs n

sublist :: (Ord a) => [a] -> [a] -> Bool
sublist _ [] = False
sublist [] _ = True
sublist xs ys = xs `elem` (splitlist ys (length xs))

partialSublist :: (Ord a) => [a] -> [a] -> Bool
partialSublist [] _ = True
partialSublist (x:xs) (y:ys)
    | x == y = partialSublist xs ys
    | otherwise = partialSublist (x:xs) ys