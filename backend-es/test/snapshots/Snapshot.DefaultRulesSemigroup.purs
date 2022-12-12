-- @inline export f never
-- @inline export g never
module Snapshot.DefaultRulesSemigroup01 where

import Prelude

f :: Int -> String
f _ = "???"

g :: Int -> String
g _ = "???"

test1 :: Int -> String
test1 = f <> g

test2 :: Int -> String
test2 = f <> g <> f <> g
