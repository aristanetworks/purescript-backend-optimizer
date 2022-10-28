module Snapshot.EffectUnsafe01 where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

foreign import random :: Effect Int

test1 :: Int
test1 = unsafePerformEffect (pure 1)

test2 :: Int
test2 = unsafePerformEffect do
  n <- random
  m <- random
  pure (n + m)
