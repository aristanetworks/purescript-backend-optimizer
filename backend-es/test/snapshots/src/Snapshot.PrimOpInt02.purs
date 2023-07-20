module Snapshot.PrimOpInt02 where

import Prelude

intValues op =
  [ 1 `op` 1
  , 1 `op` 2
  , 2 `op` 1
  , 1 `op` -2
  , -1 `op` 2
  , -1 `op` -1
  ]

test1 = intValues (+)
test2 = intValues (-)
test3 = intValues (==)
test4 = intValues (/=)
test5 = intValues (<)
test6 = intValues (>)
test7 = intValues (<=)
test8 = intValues (>=)
test9 = intValues (*)
test10 = intValues (/)
test11 = [ -1, -(-1) ]
