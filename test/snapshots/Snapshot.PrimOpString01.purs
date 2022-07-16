module Snapshot.PrimOpString01 where

import Prelude

foreign import a :: String
foreign import b :: String

test1 = a == b
test2 = a /= b
test3 = a < b
test4 = a > b
test5 = a <= b
test6 = a >= b
test7 = a <> b
