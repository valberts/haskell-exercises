module Ex4 where 

    import Test.QuickCheck
    import Data.List

    data TreeInt = Leaf
        | Node Int TreeInt TreeInt
        deriving (Show, Eq)

    insertTree :: Int -> TreeInt -> TreeInt
    insertTree n Leaf = Node n Leaf Leaf
    insertTree n (Node x t1 t2) 
        | n > x = (Node x t1 (insertTree n t2))
        | otherwise = (Node x (insertTree n t1) t2)

    makeTree :: [Int] -> TreeInt
    makeTree [] = Leaf
    makeTree [x] = (Node x Leaf Leaf)
    makeTree (x:xs) = insertTree x (makeTree xs)

    makeList :: TreeInt -> [Int]
    makeList Leaf = []
    makeList (Node x t1 t2) = makeList t1 ++ [x] ++ makeList t2

    bstSort :: [Int] -> [Int]
    bstSort [] = []
    bstSort [x] = []
    bstSort xs = makeList (makeTree xs)

    prop_bstSort xs = bstSort xs == sort xs

    sortTree :: TreeInt -> TreeInt
    sortTree Leaf = Leaf
    sortTree (Node x t1 t2) = makeTree (makeList (Node x t1 t2))