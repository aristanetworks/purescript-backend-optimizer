module Snapshot.FloatLetRegression01 where

test :: (Int -> Int) -> { b :: Int, c1 :: Int, c2 :: Int }
test f =
  ( let
      b = f 1
    in
      ( let
          c = f 2
        in
          { x: { y: { b, c1: c, c2: c } } }
      ).x
  ).y

