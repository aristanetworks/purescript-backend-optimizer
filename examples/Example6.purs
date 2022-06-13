module Example6 where

import Prelude

test1 n =
  if n == 0 then
    n
  else
    test1 (n - 1)

test2 n =
  if n == 0 then
    k false
  else
    k true
  where
  k wat = do
    let j i _ = test2 i
    if wat then j (n - 1) unit else n

test3 = go3
  where
  go3 n =
    if n == 0 then
      n
    else if n <= 100 then
      go3 (n - 1)
    else do
      k3 (n - 1)
    where
    k3 m =
      if m == 100 then
        go3 (m - 1)
      else if m == 900 then
        42
      else
        k3 (m - 1)

test4 n = if n == 1 then n else test5 (n - 1)
test5 m = if m == 2 then m else test4 (m - 2)