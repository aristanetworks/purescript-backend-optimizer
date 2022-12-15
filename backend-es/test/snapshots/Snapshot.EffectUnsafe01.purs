module Snapshot.EffectUnsafe01 where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

test1 :: Int
test1 = unsafePerformEffect (pure 1)

test2 :: Effect Int -> Int
test2 random = unsafePerformEffect do
  n <- random
  m <- random
  pure (n + m)
