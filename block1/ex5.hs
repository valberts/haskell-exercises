interest :: Float -> Float -> Float -> Float
interest a r n 
  | n > 0 = interest (a + a * r) r (n - 1)
  | n == 0 = a
