module Snapshot.Tco04 where

import Prelude

test1 :: Int -> Int
test1 n = if n == 1 then n else test2 (n - 1)

test2 :: Int -> Int
test2 m = if m == 2 then m else test1 (m - 2)
