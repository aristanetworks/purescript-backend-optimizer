module Snapshot.CaseRedBlackTree where

data RedBlackTree a
  = Leaf
  | Node Color (RedBlackTree a) a (RedBlackTree a)

data Color = Red | Black

test1
  :: Partial
  => RedBlackTree Int
  -> { i :: Int
     , a :: RedBlackTree Int
     , x :: Int
     , b :: RedBlackTree Int
     , y :: Int
     , c :: RedBlackTree Int
     , z :: Int
     , d :: RedBlackTree Int
     }
test1 = case _ of
  Node Black (Node Red (Node Red a x b) y c) z d -> { i: 1, a, x, b, y, c, z, d }
  Node Black (Node Red a x (Node Red b y c)) z d -> { i: 2, a, x, b, y, c, z, d }
  Node Black a x (Node Red (Node Red b y c) z d) -> { i: 3, a, x, b, y, c, z, d }
  Node Black a x (Node Red b y (Node Red c z d)) -> { i: 4, a, x, b, y, c, z, d }
