module Snapshot.PrimOpInt01 where

import Prelude

test1 :: Int -> Int -> Int
test1 a b = a + b

test2 :: Int -> Int -> Int
test2 a b = a - b

test3 :: Int -> Int -> Boolean
test3 a b = a == b

test4 :: Int -> Int -> Boolean
test4 a b = a /= b

test5 :: Int -> Int -> Boolean
test5 a b = a < b

test6 :: Int -> Int -> Boolean
test6 a b = a > b

test7 :: Int -> Int -> Boolean
test7 a b = a <= b

test8 :: Int -> Int -> Boolean
test8 a b = a >= b

test9 :: Int -> Int -> Int
test9 a b = a * b

test10 :: Int -> Int -> Int
test10 a b = a / b

test11 :: Int -> Int
test11 a = -a
