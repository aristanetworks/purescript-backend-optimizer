module Snapshot.PrimOpBoolean01 where

import Prelude

test1 :: Boolean -> Boolean -> Boolean
test1 a b = a && b

test2 :: Boolean -> Boolean -> Boolean
test2 a b = a || b

test3 :: Boolean -> Boolean -> Boolean
test3 a b = a == b

test4 :: Boolean -> Boolean -> Boolean
test4 a b = a /= b

test5 :: Boolean -> Boolean -> Boolean
test5 a b = a < b

test6 :: Boolean -> Boolean -> Boolean
test6 a b = a > b

test7 :: Boolean -> Boolean -> Boolean
test7 a b = a <= b

test8 :: Boolean -> Boolean -> Boolean
test8 a b = a >= b

test9 :: Boolean -> Boolean
test9 a = not a
