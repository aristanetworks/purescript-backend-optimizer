module Snapshot.DefaultRulesMonoid01 where

import Prelude
import Data.Monoid (guard)

type F = forall a. a -> a

test1 :: Boolean -> Array Int
test1 = flip guard [ 1, 2, 3 ]

test2 :: F -> Boolean -> Array Int
test2 f = flip guard (f [ 1, 2, 3 ])
