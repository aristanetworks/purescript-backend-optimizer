-- @inline export f never
-- @inline export g never
module Snapshot.FunctionCompose03 (test1, test2, test3, test4) where

import Prelude

f :: Unit -> Int -> Int
f _ = identity

g :: Unit -> Int -> Int
g _ = identity

test1 :: Int -> Int
test1 = f unit <<< g unit

test2 :: Int -> Int
test2 = g unit <<< (f unit <<< g unit)

test3 :: Int -> Int
test3 = (f unit <<< g unit) <<< (f unit <<< g unit)

test4 :: Int -> Int
test4 = ((g unit <<< f unit) <<< g unit) <<< (f unit <<< g unit)
