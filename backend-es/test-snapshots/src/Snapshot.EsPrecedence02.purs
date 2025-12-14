module Snapshot.EsPrecedence02 where

import Prelude

test1 :: Number -> Number
test1 a = a + (a + (a + a))

test2 :: Number -> Number
test2 a = ((a + a) + a) + a

test3 :: Number -> Number
test3 a = a + (a + (a - a))

test4 :: Number -> Number
test4 a = ((a - a) + a) + a

test5 :: Number -> Number
test5 a = (a - a) + (a + a)
