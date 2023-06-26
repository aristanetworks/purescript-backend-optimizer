-- @inline Snapshot.RecursionInlinedBroken.addStuffBroken always
-- This will recurse out of control and stop when it hits the recursion limit
module Snapshot.RecursionInlinedBroken where

import Prelude

data List a = Nil | Cons a (List a)
infixr 5 Cons as :

addStuffBroken :: Int -> Int -> Int
addStuffBroken 0 ys = ys
addStuffBroken x ys = 1 + addStuffBroken (x - 1) ys

infixr 4 addStuffBroken as ++

test1 :: Int
test1 = (-3) ++ 4
test2 :: Int -> Int
test2 z = (-3) ++ z ++ 5