import Debug.Trace

debug = flip trace

increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) = x < y && increasing (y:xs)

weaklyIncreasing :: [Int] -> Bool
weaklyIncreasing [] = True
weaklyIncreasing [x] = True
weaklyIncreasing xs = helper xs []

helper :: [Int] -> [Int] -> Bool
helper (x:xs) ys
    | length ys < 1 = helper xs (x:ys)
    | length xs < 1 = True
    | otherwise = average ys < x && helper xs (x:ys)

average :: Foldable t => t Int -> Int
average xs = (sum xs) `div` (length xs)