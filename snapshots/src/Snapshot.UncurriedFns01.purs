module Snapshot.UncurriedFns01 where

import Data.Function.Uncurried (Fn3, runFn3)

type F = Fn3 Int Int Int Int
type G = Int -> Int

test1 :: F -> G -> Int
test1 f g = runFn3 f (g 1) 2 3

test2 :: F -> G -> Int -> Int
test2 f g = runFn3 f (g 1) 2

test3 :: F -> G -> Int -> Int -> Int
test3 f g = runFn3 f (g 1)

test4 :: F -> Int -> Int -> Int -> Int
test4 f = runFn3 f
