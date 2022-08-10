module Snapshot.DefaultRulesSemigroup01 where

import Prelude

foreign import f :: Int -> String
foreign import g :: Int -> String

test1 = f <> g
test2 = f <> g <> f <> g
