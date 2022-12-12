-- @inline export f never
-- @inline export g never
module Snapshot.FunctionCompose02 (test1, test2, test3, test4) where

import Prelude

f :: Int -> Int
f = identity

g :: Int -> Int
g = identity

test1 :: Int -> Int
test1 = f <<< g

test2 :: Int -> Int
test2 = g <<< (f <<< g)

test3 :: Int -> Int
test3 = (f <<< g) <<< (f <<< g)

test4 :: Int -> Int
test4 = ((g <<< f) <<< g) <<< (f <<< g)
