module Snapshot.PrimOpChar02 where

import Prelude

charValues op =
  [ 'a' `op` 'a'
  , 'a' `op` 'b'
  , 'b' `op` 'a'
  ]

test1 = charValues (==)
test2 = charValues (/=)
test3 = charValues (<)
test4 = charValues (>)
test5 = charValues (<=)
test6 = charValues (>=)
