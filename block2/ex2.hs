import Ex1

treeAdd :: Tree1a -> Int -> Tree1a
treeAdd (Leaf1a n) x = (Leaf1a (n + x))
treeAdd (Node1a n t1 t2) x = (Node1a (n + x) (treeAdd t1 x) (treeAdd t2 x))

treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a n) = (Leaf1a (n * n))
treeSquare (Node1a n t1 t2) = (Node1a (n * n) (treeSquare t1) (treeSquare t2))

mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) = (Leaf1a (f n))
mapTree f (Node1a n t1 t2) = (Node1a (f n) (mapTree f t1) (mapTree f t2))

treeAdd' :: Tree1a -> Int -> Tree1a
treeAdd' t x = mapTree (+x) t

treeSquare' :: Tree1a -> Tree1a
treeSquare' t = mapTree (^2) t

addNode :: Tree1b -> Tree1a
addNode (Leaf1b (n, m)) = (Leaf1a (n + m))
addNode (Node1b (n, m) t1 t2) = (Node1a (n + m) (addNode t1) (addNode t2))

mapTree1b :: ((Int, Int) -> Int) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b (n, m)) = (Leaf1a (f (n, m)))
mapTree1b f (Node1b (n, m) t1 t2) = (Node1a (f (n, m)) (mapTree1b f t1) (mapTree1b f t2))

addTree1b :: Tree1b -> Tree1a
addTree1b t = mapTree1b (\(n, m) -> n + m) t

multTree1b :: Tree1b -> Tree1a
multTree1b t = mapTree1b (\(n, m) -> n * m) t