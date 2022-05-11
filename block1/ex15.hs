isPythTriple :: (Num a, Eq a) => (a, a, a) -> Bool
isPythTriple (a, b, c) = a^2 + b^2 == c^2