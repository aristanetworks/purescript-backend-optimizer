module Snapshot.PrimOpBoolean02 where

import Prelude

boolValues op =
  [ op true true
  , op true false
  , op false true
  , op false false
  ]

test1 = boolValues (&&)
test2 = boolValues (||)
test3 = boolValues (==)
test4 = boolValues (/=)
test5 = boolValues (<)
test6 = boolValues (>)
test7 = boolValues (<=)
test8 = boolValues (>=)
test9 = [ not true, not false ]
