module Snapshot.Tco01 where

import Prelude

test :: Int -> Int
test n =
  if n == 0 then n
  else test (n - 1)
