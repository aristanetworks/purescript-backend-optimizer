module Snapshot.DefaultRulesSemigroup02 where

import Prelude

type R = { foo :: String, bar :: Array String }

foreign import x :: Unit -> String

test1 :: R -> R -> R
test1 = append

test2 :: R -> R -> R
test2 a b = append a b

test3 :: R -> R
test3 = append { foo: "hello", bar: [ "hello" ] }

test4 :: R
test4 = append { foo: "hello", bar: [ "hello" ] } { foo: ", World!", bar: [ "World!" ] }
