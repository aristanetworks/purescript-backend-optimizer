module Snapshot.STArray02 where

import Prelude

import Data.Array.ST as STArray

test1 :: Array Int -> Array Int
test1 inp = STArray.run do
  arr <- STArray.new
  n <- STArray.push 1 arr
  _ <- STArray.pushAll [ 1, n ] arr
  _ <- STArray.pushAll inp arr
  _ <- STArray.pushAll ([1, 2, 3] <> inp) arr
  _ <- STArray.pushAll (inp <> [2, 3, 4]) arr
  _ <- STArray.pushAll ([1, 2, 3] <> inp <> [5, 6, 7]) arr
  pure arr

test2 :: Array Int -> Array Int
test2 inp = STArray.run do
  arr <- STArray.new
  n <- STArray.unshift 1 arr
  _ <- STArray.unshiftAll [ 1, n ] arr
  _ <- STArray.unshiftAll inp arr
  _ <- STArray.unshiftAll ([1, 2, 3] <> inp) arr
  _ <- STArray.unshiftAll (inp <> [2, 3, 4]) arr
  _ <- STArray.unshiftAll ([1, 2, 3] <> inp <> [5, 6, 7]) arr
  pure arr
