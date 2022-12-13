module Snapshot.DefaultRulesSemigroup01 where

import Prelude

type F = Int -> String

test1 :: F -> F -> Int -> String
test1 f g = f <> g

test2 :: F -> F -> Int -> String
test2 f g = f <> g <> f <> g
