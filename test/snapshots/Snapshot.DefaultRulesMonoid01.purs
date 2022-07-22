module Snapshot.DefaultRulesMonoid01 where

import Prelude
import Data.Monoid (guard)

foreign import f :: forall a. a -> a

test1 = flip guard [ 1, 2, 3 ]
test2 = flip guard (f [ 1, 2, 3 ])
