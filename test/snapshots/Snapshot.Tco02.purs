module Snapshot.Tco02 where

import Prelude

test :: Int -> Int
test n =
  if n == 0 then k false
  else k true
  where
  k wat = do
    let j i _ = test i
    if wat then j (n - 1) unit
    else n
