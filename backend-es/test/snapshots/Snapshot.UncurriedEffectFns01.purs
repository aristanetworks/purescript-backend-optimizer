module Snapshot.UncurriedEffectFns01 where

import Prelude

import Effect.Uncurried (EffectFn3, runEffectFn3)

foreign import f :: forall a b c. EffectFn3 a b c Unit

foreign import g :: forall a. a -> a

test1 = runEffectFn3 f (g 1) 2 3
test2 = runEffectFn3 f (g 1) 2
test3 = runEffectFn3 f (g 1)
test4 = runEffectFn3 f

test5 = do
  runEffectFn3 f (g 1) 2 3
  runEffectFn3 f (g 1) 2 3

test6 = do
  runEffectFn3 f (g 1) 2 3
  runEffectFn3 f (g 1) 2 3
  runEffectFn3 f (g 1) 2 3

