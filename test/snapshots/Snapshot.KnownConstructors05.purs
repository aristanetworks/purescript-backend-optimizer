-- @inline export fromString arity=1
module Snapshot.KnownConstructors05 where

import Data.Maybe (Maybe(..))

data Test = Foo | Bar | Baz | Qux

fromString :: String -> Maybe Test
fromString = case _ of
  "foo" -> Just Foo
  "bar" -> Just Bar
  "baz" -> Just Baz
  "qux" -> Just Qux
  _ -> Nothing

test :: String -> Int
test a = case fromString a of
  Just Foo -> 1
  Just Bar -> 2
  Just Baz -> 3
  Just Qux -> 4
  Nothing -> 0
