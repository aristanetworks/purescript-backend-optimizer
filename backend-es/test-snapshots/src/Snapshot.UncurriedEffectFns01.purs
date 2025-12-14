module Snapshot.UncurriedEffectFns01 where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)

type F = EffectFn3 Int Int Int Unit
type G = Int -> Int

test1 :: F -> G -> Effect Unit
test1 f g = runEffectFn3 f (g 1) 2 3

test2 :: F -> G -> Int -> Effect Unit
test2 f g = runEffectFn3 f (g 1) 2

test3 :: F -> G -> Int -> Int -> Effect Unit
test3 f g = runEffectFn3 f (g 1)

test4 :: F -> Int -> Int -> Int -> Effect Unit
test4 f = runEffectFn3 f

test5 :: F -> G -> Effect Unit
test5 f g = do
  runEffectFn3 f (g 1) 2 3
  runEffectFn3 f (g 1) 2 3

test6 :: F -> G -> Effect Unit
test6 f g = do
  runEffectFn3 f (g 1) 2 3
  runEffectFn3 f (g 1) 2 3
  runEffectFn3 f (g 1) 2 3

