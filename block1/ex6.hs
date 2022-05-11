discr :: Float -> Float -> Float -> Float
discr a b c 
  | a == 0 = error "a cannot be 0"
  | b ^ 2 - 4 * a * c < 0 = error "negative discriminant"
  | otherwise = b ^ 2 - 4 * a * c

root1 :: Float -> Float -> Float -> Float
root1 a b c = ((-b) + sqrt(discr a b c)) / 2 * a

root2 :: Float -> Float -> Float -> Float
root2 a b c = ((-b) - sqrt(discr a b c)) / 2 * a