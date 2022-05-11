myflip :: (a -> b -> c) -> b -> a -> c
myflip = (\f a b -> f b a)