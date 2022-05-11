{-# LANGUAGE TemplateHaskell #-}
module Set3 where

import FPPrac.Trees
import Data.Maybe
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.All



data Tree1a = Leaf1a Int
            | Node1a Int Tree1a Tree1a
            deriving (Show, Eq)

-- Test with: generate arbitrary :: IO (Tree1a)
instance Arbitrary Tree1a where
  arbitrary = sized arbtree1a

arbtree1a :: Int -> Gen Tree1a
arbtree1a 0 = Leaf1a <$> arbitrary
arbtree1a n = frequency
   [ (1, Leaf1a <$> arbitrary),
     (2, Node1a <$> arbitrary
                <*> (arbtree1a (n `div` 2))
                <*> (arbtree1a (n `div` 2))) ]

-- Example of a property for a later exercise
--prop_tree1amirror :: Tree1a -> Bool
--prop_tree1amirror t = (binMirror1a . binMirror1a) t == t








data Tree1b = Leaf1b (Int,Int)
            | Node1b (Int,Int) Tree1b Tree1b
            deriving (Show, Eq)

instance Arbitrary Tree1b where
  arbitrary = sized arbtree1b

arbtree1b :: Int -> Gen Tree1b
arbtree1b 0 = Leaf1b <$> arbitrary
arbtree1b n = frequency
   [ (1, Leaf1b <$> arbitrary),
     (2, Node1b <$> arbitrary
                <*> (arbtree1b (n `div` 2))
                <*> (arbtree1b (n `div` 2))) ]





data Tree1c = Leaf1c Int
            | Node1c Tree1c Tree1c
            deriving (Show, Eq)

instance Arbitrary Tree1c where
  arbitrary = sized arbtree1c

arbtree1c :: Int -> Gen Tree1c
arbtree1c 0 = Leaf1c <$> arbitrary
arbtree1c n = frequency
   [ (1, Leaf1c <$> arbitrary),
     (2, Node1c <$> (arbtree1c (n `div` 2))
                <*> (arbtree1c (n `div` 2))) ]





data Tree1d = Leaf1d (Int,Int)
            | Node1d [Tree1d]
            deriving (Show, Eq)

instance Arbitrary Tree1d where
  arbitrary = sized arbtree1d

arbtree1d :: Int -> Gen Tree1d
arbtree1d 0 = Leaf1d <$> arbitrary
arbtree1d n = do m <- choose (0, n)
                 if (m == 0) then
                    do k <- choose (1,10)
                       Node1d <$> vectorOf k (Leaf1d <$> arbitrary)
                 else
                    Node1d <$> resize (n `div` m) arbitrary





data BinTree a = Leaf
               | Node a (BinTree a) (BinTree a)
               deriving (Show, Eq)


instance (Arbitrary a) => Arbitrary (BinTree a) where
  arbitrary = sized arbbintree

arbbintree :: Arbitrary a => Int -> Gen (BinTree a)
arbbintree 0 = pure Leaf
arbbintree n = frequency
   [ (1, pure Leaf),
     (2, Node <$> arbitrary
              <*> (arbbintree (n `div` 2))
              <*> (arbbintree (n `div` 2))) ]



--instance (Arbitrary a) => Arbitrary (MyList a) where
--  arbitrary = fromList <$> arbitrary


