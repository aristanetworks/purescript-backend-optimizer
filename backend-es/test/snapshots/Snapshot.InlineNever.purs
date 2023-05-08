-- @inline Snapshot.InlineNever.foo never
module Snapshot.InlineNever where

foo :: String
foo = "foo"

test :: String
test = foo
