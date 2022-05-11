module Ex1 where 
    
  import FPPrac.Trees

  data Tree1a = Leaf1a Int
    | Node1a Int Tree1a Tree1a
    deriving (Show, Eq)

  tree1aExampleTree :: Tree1a
  tree1aExampleTree = Node1a 1 (Leaf1a 2) (Leaf1a 3)

  pp1a :: Tree1a -> RoseTree
  pp1a (Leaf1a n) = (RoseNode (show n) [])
  pp1a (Node1a n t1 t2) = (RoseNode (show n) [pp1a t1, pp1a t2])

  data Tree1b = Leaf1b (Int, Int)
    | Node1b (Int, Int) Tree1b Tree1b
    deriving (Show, Eq)

  tree1bExampleTree :: Tree1b
  tree1bExampleTree = Node1b (1, 1) (Leaf1b (2, 2)) (Leaf1b (3, 3))

  pp1b :: Tree1b -> RoseTree
  pp1b (Leaf1b (n, m)) = (RoseNode (show (n, m)) [])
  pp1b (Node1b (n, m) t1 t2) = (RoseNode (show (n, m)) [pp1b t1, pp1b t2])

  data Tree1c = Leaf1c Int
    | Node1c Tree1c Tree1c
    deriving (Show, Eq)

  tree1cExampleTree :: Tree1c
  tree1cExampleTree = Node1c (Leaf1c 1) (Leaf1c 2)

  pp1c :: Tree1c -> RoseTree
  pp1c (Leaf1c n) = (RoseNode (show n) [])
  pp1c (Node1c t1 t2) = (RoseNode [] [pp1c t1, pp1c t2])

  data Tree1d = Leaf1d (Int, Int)
    | Node1d [Tree1d]
    deriving (Show, Eq)

  tree1dExampleTree :: Tree1d
  tree1dExampleTree = Node1d [Leaf1d (1, 2), Leaf1d (2, 3)]

  pp1d :: Tree1d -> RoseTree
  pp1d (Leaf1d (n, m)) = (RoseNode (show (n, m)) [])
  pp1d (Node1d xs) = (RoseNode [] [pp1d x | x <- xs])

  class PP a where
    pp :: a -> RoseTree

  instance PP Tree1a where
    pp t = pp1a t
  instance PP Tree1b where
    pp t = pp1b t
  instance PP Tree1c where
    pp t = pp1c t
  instance PP Tree1d where
    pp t = pp1d t