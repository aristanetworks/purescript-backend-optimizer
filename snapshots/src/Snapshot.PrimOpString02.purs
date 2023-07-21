module Snapshot.PrimOpString02 where

import Prelude

stringValues op =
  [ "a" `op` "a"
  , "a" `op` "b"
  , "b" `op` "a"
  ]

test1 = stringValues (==)
test2 = stringValues (/=)
test3 = stringValues (<)
test4 = stringValues (>)
test5 = stringValues (<=)
test6 = stringValues (>=)
test7 = stringValues (<>)
