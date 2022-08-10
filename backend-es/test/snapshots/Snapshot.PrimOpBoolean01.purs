module Snapshot.PrimOpBoolean01 where

import Prelude

foreign import a :: Boolean
foreign import b :: Boolean

test1 = a && b
test2 = a || b
test3 = a == b
test4 = a /= b
test5 = a < b
test6 = a > b
test7 = a <= b
test8 = a >= b
test9 = not a
