module Snapshot.PrimOpNumber01 where

import Prelude

test1 :: Number -> Number -> Number
test1 a b = a + b

test2 :: Number -> Number -> Number
test2 a b = a - b

test3 :: Number -> Number -> Boolean
test3 a b = a == b

test4 :: Number -> Number -> Boolean
test4 a b = a /= b

test5 :: Number -> Number -> Boolean
test5 a b = a < b

test6 :: Number -> Number -> Boolean
test6 a b = a > b

test7 :: Number -> Number -> Boolean
test7 a b = a <= b

test8 :: Number -> Number -> Boolean
test8 a b = a >= b

test9 :: Number -> Number -> Number
test9 a b = a * b

test10 :: Number -> Number -> Number
test10 a b = a / b

test11 :: Number -> Number
test11 a = -a

