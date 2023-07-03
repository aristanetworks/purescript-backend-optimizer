-- @inline Snapshot.RecursionInlined02.addStuff always
module Snapshot.RecursionInlined02 where

import Prelude

data List a = Nil | Cons a (List a)
infixr 5 Cons as :

addStuff :: Int -> Int -> Int
addStuff x ys
  | x > 0 = 1 + addStuff (x - 1) ys
  | x < 0 = (-1) + addStuff (x + 1) ys
  | otherwise = ys

infixr 4 addStuff as ++

test1 :: Int
test1 = 38 ++ 4
test2 :: Int -> Int
test2 z = 3 ++ z ++ 5