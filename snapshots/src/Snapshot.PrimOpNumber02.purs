module Snapshot.PrimOpNumber02 where

import Prelude

numValues op =
  [ 1.5 `op` 1.0
  , 1.5 `op` 2.0
  , 2.5 `op` 1.0
  , 1.5 `op` -2.0
  , -1.5 `op` 2.0
  , -1.5 `op` -1.0
  ]

test1 = numValues (+)
test2 = numValues (-)
test3 = numValues (==)
test4 = numValues (/=)
test5 = numValues (<)
test6 = numValues (>)
test7 = numValues (<=)
test8 = numValues (>=)
test9 = numValues (*)
test10 = numValues (/)
test11 = [ -1.5, -(-1.5) ]
