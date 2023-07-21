module Snapshot.UncurriedLocalAbs01 where

import Prelude

import Data.Foldable (sum)

test :: Int -> Int -> Int
test x y =
  fn x y + fn y x
  where
  fn a b =
    sum [ x, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b ]
