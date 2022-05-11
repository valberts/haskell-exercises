import Ex1

binMirror1a :: Tree1a -> Tree1a
binMirror1a (Leaf1a n) = (Leaf1a n)
binMirror1a (Node1a n t1 t2) = (Node1a n t2 t1)

class BinMirror a where
  binMirror :: a -> a

instance BinMirror Tree1a where
  binMirror t = binMirror1a t

instance BinMirror Tree1d where
  binMirror (Leaf1d (n, m)) = (Leaf1d (m, n))
  binMirror (Node1d xs) = (Node1d [binMirror x | x <- last xs : (init . tail $ xs) ++ [head xs] ])