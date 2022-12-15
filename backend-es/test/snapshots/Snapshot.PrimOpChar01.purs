module Snapshot.PrimOpChar01 where

import Prelude

test1 :: Char -> Char -> Boolean
test1 a b = a == b

test2 :: Char -> Char -> Boolean
test2 a b = a /= b

test3 :: Char -> Char -> Boolean
test3 a b = a < b

test4 :: Char -> Char -> Boolean
test4 a b = a > b

test5 :: Char -> Char -> Boolean
test5 a b = a <= b

test6 :: Char -> Char -> Boolean
test6 a b = a >= b
