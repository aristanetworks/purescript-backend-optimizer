module Snapshot.Tco07 where

import Prelude

f :: Int -> Int -> Unit
f = (\a b -> j (a + b))
  where
  j n
    | n == 100 = g n
    | otherwise = j (n + 1)

g :: Int -> Unit
g n = f n (n - 1)
