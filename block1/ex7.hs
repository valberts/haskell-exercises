extrX :: (Fractional a) => a -> a -> a -> a
extrX a b c = -b / (2 * a)

extrY :: (Fractional a) => a -> a -> a -> a
extrY a b c = a * x ^ 2 + b * x + c
  where x = extrX a b c