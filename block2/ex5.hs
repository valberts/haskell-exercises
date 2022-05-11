import Data.Maybe
import Ex4

subtreeAt :: TreeInt -> Int -> Maybe TreeInt
subtreeAt Leaf n = Nothing
subtreeAt (Node x t1 t2) n
    | n == x = (Node x t1 t2)
    | n > x = subtreeAt t1 n
    | otherwise = subtreeAt t2 n