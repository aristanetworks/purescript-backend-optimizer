module Snapshot.Tco03 where

import Prelude

test :: Int -> Int
test = go
  where
  go n =
    if n == 0 then n
    else if n <= 100 then go (n - 1)
    else do k (n - 1)
    where
    k m =
      if m == 100 then go (m - 1)
      else if m == 900 then 42
      else k (m - 1)

test2 :: Int -> Int
test2 n =
  if n == 0 then n
  else if n <= 100 then test2 (n - 1)
  else do k (n - 1)
  where
  k m =
    if m == 100 then test2 (m - 1)
    else if m == 900 then 42
    else k (m - 1)
