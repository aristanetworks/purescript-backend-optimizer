module Snapshot.PrimOpString01 where

import Prelude

test1 :: String -> String -> Boolean
test1 a b = a == b

test2 :: String -> String -> Boolean
test2 a b = a /= b

test3 :: String -> String -> Boolean
test3 a b = a < b

test4 :: String -> String -> Boolean
test4 a b = a > b

test5 :: String -> String -> Boolean
test5 a b = a <= b

test6 :: String -> String -> Boolean
test6 a b = a >= b

test7 :: String -> String -> String
test7 a b = a <> b
