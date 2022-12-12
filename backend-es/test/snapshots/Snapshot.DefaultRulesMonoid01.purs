-- @inline export f never
module Snapshot.DefaultRulesMonoid01 where

import Prelude
import Data.Monoid (guard)

f :: forall a. a -> a
f = identity

test1 :: Boolean -> Array Int
test1 = flip guard [ 1, 2, 3 ]

test2 :: Boolean -> Array Int
test2 = flip guard (f [ 1, 2, 3 ])
