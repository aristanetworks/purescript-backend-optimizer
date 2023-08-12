module Snapshot.AssocIntOps where

import Prelude

test1 :: Int -> Int
test1 x = 1 + (((((2 + x) + x) + x) + x) + 3) + 4

test2 :: Int -> Int
test2 x = 1 + (2 + (x + (x + (x + (x + 3))))) + 4

test3 :: Int -> Int
test3 x = 1 + (2 + (x + (x + (x + (x + 3))))) + 4 + (((((5 + x) + x) + x) + x) + 6) + 7

test4 :: Int -> Int
test4 x = 1 * (((((2 * x) * x) * x) * x) * 3) * 4

test5 :: Int -> Int
test5 x = 1 * (2 * (x * (x * (x * (x * 3))))) * 4

test6 :: Int -> Int
test6 x = 1 * (2 * (x * (x * (x * (x * 3))))) * 4 * (((((5 * x) * x) * x) * x) * 6) * 7
