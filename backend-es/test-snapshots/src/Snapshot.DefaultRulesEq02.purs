module Snapshot.DefaultRulesEq02 where

import Prelude

test1 :: Int -> Int -> Boolean
test1 = notEq

test2 :: Int -> Int -> Boolean
test2 a b = notEq a b

test3 :: Int -> Boolean
test3 a = notEq 12 a

test4 :: Int -> Boolean
test4 a = notEq a 12

test5 :: Int -> Boolean
test5 = notEq 12

type R = { foo :: Int, bar :: String, baz :: Boolean }

test6 :: R -> Boolean
test6 = notEq { foo: 42, bar: "hello", baz: false }
