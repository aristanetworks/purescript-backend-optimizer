module Snapshot.DefaultRulesEq01 where

import Prelude

type R = { foo :: Int, bar :: String, baz :: Boolean }

test1 :: R -> R -> Boolean
test1 = eq

test2 :: R -> R -> Boolean
test2 a b = eq a b

test3 :: R -> Boolean
test3 = eq { foo: 42, bar: "hello", baz: false }

test4 :: R -> Boolean
test4 a = eq { foo: 42, bar: "hello", baz: false } a

test5 :: R -> Boolean
test5 a = eq a { foo: 42, bar: "hello", baz: false }

test6 :: Boolean
test6 = eq { foo: 42, bar: "hello", baz: false } { foo: 42, bar: "hello", baz: false }

test7 :: Boolean
test7 = eq { foo: 42, bar: "hello", baz: false } { foo: 43, bar: "hello", baz: false }

test8 :: (Unit -> String) -> Boolean
test8 x = eq { foo: 42, bar: "hello", baz: false } { foo: 43, bar: x unit, baz: false }

test9 :: (Unit -> String) -> Boolean
test9 x = eq { foo: 42, bar: "hello", baz: true } { foo: 42, bar: x unit, baz: true }

test10 :: (Unit -> String) -> R -> Boolean
test10 x = eq { foo: 42, bar: x unit, baz: true }
