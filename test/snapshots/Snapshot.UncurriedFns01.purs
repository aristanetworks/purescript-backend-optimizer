module Snapshot.UncurriedFns01 where

import Data.Function.Uncurried (Fn3, runFn3)

foreign import f :: forall a b c d. Fn3 a b c d

foreign import g :: forall a. a -> a

test1 = runFn3 f (g 1) 2 3
test2 = runFn3 f (g 1) 2
test3 = runFn3 f (g 1)
test4 = runFn3 f
